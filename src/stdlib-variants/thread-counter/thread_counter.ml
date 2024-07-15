open Common

let add_err (e : 'e) = function Error es -> Error (es @ [e]) | Ok _ -> Error [e]

module Debug = struct
  let disabled_ fmt = Stdlib.(Printf.ifprintf stderr fmt)

  let enabled_ fmt =
    Stdlib.(
      Printf.ksprintf
        (fun s ->
          prerr_string s;
          prerr_newline ())
        fmt)

  let d_ = disabled_
  let i_ = enabled_
end

open Debug

module ThreadH = Hashtbl.Make (struct
  type t = Thread.t

  let equal t1 t2 = compare (Thread.id t1) (Thread.id t2) = 0
  let hash t = Hashtbl.hash (Thread.id t)
end)

module Counter = struct

  open Ctx_util
  open Ctx_util.Syntax

  let lock_if b m = if b then Mutex.protect m else empty_context' ()

  (** Note: we enforce that spawned threads don't raise uncaught exceptions,
     which in theory changes the semantics of threads. The value of being
     able to report stray exceptions outweighs the slim chance anyone
     would rely on being able to ignore exceptions in threads. *)

  type 'a finished = Return of 'a | Uncaught of Util.exn_info | Overflow of int
  type 'a state = Running | Finished of 'a finished

  type 'a group = {
    mutable state : 'a state;
    finished : Condition.t; (* predicate: state <> Running, mutex: owner.mut *)
    mutable thread_count : int;
    thread_limit : int option;
    owner : t;
  }

  and g = G : 'a group -> g
  and t = { mut : Mutex.t; groups : g ThreadH.t }

  let finish ?(lock = true) group fin =
    d_ "finish";

    let< _ = lock_if lock group.owner.mut in
    if group.state <> Running then failwith "finish: already finished";
    group.state <- Finished fin;
    Condition.broadcast group.finished

  let try_finish group fin =
    let< _ = Mutex.protect group.owner.mut in
    if group.state = Running then
    finish ~lock:false group fin

  let try_return group x = try_finish group (Return x)

  let spawn_thread ?(lock = true) ?group cnt (f : _ -> unit) x =
    d_ "create_thread";

    let make_thread group f x =
      let cnt = group.owner in
      let tid =
        Thread.create
          (fun () ->
            d_ "thread#%d started" Thread.(self () |> id);
            Fun.protect
              (fun () ->
                Util.try_to_result f x
                |> Result.iter_error (fun e -> try_finish group (Uncaught e)))
              ~finally:(fun () ->
                let< _ = Mutex.protect cnt.mut in
                let tid = Thread.self () in
                group.thread_count <- group.thread_count - 1;
                ThreadH.remove cnt.groups tid;
                d_ "thread#%d finished" Thread.(self () |> id)))
          ()
      in
      ThreadH.replace cnt.groups tid (G group);
      group.thread_count <- group.thread_count + 1;
      tid
    in

    let spawn = fun group ->
      match group.state, group.thread_limit with
      | Running, Some limit when group.thread_count >= limit ->
          finish ~lock:false group (Overflow limit); Thread.self ()
      | Running, _ -> make_thread group f x
      | Finished _, _ -> Thread.self ()
    in

    let< _ = lock_if lock cnt.mut in
    match group with
    | Some g -> spawn g
    | None -> let G g = ThreadH.find cnt.groups (Thread.self ()) in spawn g

  let create_counter () = { mut = Mutex.create (); groups = ThreadH.create 32 }

  let create_group ?thread_limit cnt =
    d_ "create_group";
    (* no lock needed, since we don't write to the owner;
       operations on the group will lock instead *)
    {
      state = Running;
      finished = Condition.create ();
      thread_count = 0;
      thread_limit;
      owner = cnt;
    }

  let get_thread_count group =
    let< _ = Mutex.protect group.owner.mut in group.thread_count

  (** Wait for threads in a group to complete. Group must be finished first. *)
  let join_group ~leftover_thread_limit ~timeout group =
    (* busy waits to implement timeout *)
    d_ "join_group";

    let _ =
      let< _ = Mutex.protect group.owner.mut in
      if group.state = Running then failwith "join_group: still running"
    in

    let time0 = Mtime_clock.counter () in
    let rec loop () =
      let remaining = get_thread_count group in
      if remaining <= leftover_thread_limit then remaining
      else if Mtime.Span.compare (Mtime_clock.count time0) timeout >= 0 then remaining
      else (Thread.yield (); loop ())
    in
    loop ()

  (** {1 High-level group operations} *)

  type thread_group_err =
    | ThreadLimitReached of int
    | ThreadsLeftOver of { left_over : int; limit : int }
    | ExceptionRaised of { main : bool; exn_info : Util.exn_info }

  (** Create a group that runs the given function, then sets the return value
      as the return value of the group. *)
  let spawn_thread_group ?thread_limit cnt f x =
    let group = create_group ?thread_limit cnt in
    spawn_thread ~group cnt Util.(try_return group % try_to_result f) x |> ignore;
    group

  (** Wait for a group to finish then join its threads ({!join_group}). *)
  let collect_thread_group ?leftover_limit group =
    let cnt = group.owner in
    let fin =
      let rec loop () = match group.state with
        | Running -> d_ "still running"; Condition.wait group.finished cnt.mut; loop ()
        | Finished fin -> fin
      in
      let< _ = Mutex.protect cnt.mut in loop ()
    in

    let leftover_count =
      match leftover_limit with
      | Some (n, t) -> join_group ~leftover_thread_limit:n ~timeout:t group
      | None -> get_thread_count group
    in

    fin, leftover_count

  (** Check the return value of {!collect_thread_group} *)
  let check_thread_group_result ?leftover_limit fin leftover_count =
    let r = match fin with
      | Return (Ok x) -> Ok x
      | Return (Error e) -> Error [ExceptionRaised { main = true; exn_info = e }]
      | Uncaught e -> Error [ExceptionRaised { main = false; exn_info = e }]
      | Overflow l -> Error [ThreadLimitReached l]
    in

    match leftover_limit with
      | Some (lim, (_ : Mtime.span)) when leftover_count > lim ->
          r |> add_err (ThreadsLeftOver { left_over = leftover_count ; limit = lim })
      | _ -> r

  (** Combines {!spawn_thread_group}, {!collect_thread_group}, and {!check_thread_group_result}.*)
  let run_in_thread_group ?thread_limit ?leftover_limit cnt f x =
    let group = spawn_thread_group cnt f x ?thread_limit in
    let fin, leftover_count = collect_thread_group group ?leftover_limit in
    check_thread_group_result ?leftover_limit fin leftover_count

  (* the rest is for error reporting, could do with less code/abstraction... *)

  let string_of_thread_group_err = function
    | ThreadLimitReached n ->
        Printf.sprintf "Too many threads were used (> %d)" n
    | ThreadsLeftOver { left_over = n; limit } ->
        Printf.sprintf "Too many threads were left running (%d > %d)" n limit
    | ExceptionRaised { main; exn_info = Util.{exn; backtrace} } ->
        Printf.sprintf "%s thread raised an exception: %s\n%s"
          (if main then "The main" else "A created")
          (Printexc.to_string exn) (Printexc.raw_backtrace_to_string backtrace)
        |> String.trim

  let string_of_thread_group_errs = function
    | [] -> "Unknown error in a thread group" (* this shouldn't happen *)
    | [err] -> "Error in a thread group: " ^ string_of_thread_group_err err
    | errs ->
        "Multiple errors in a thread group:\n" ^
        (errs |> List.mapi (fun i err -> [
            Printf.sprintf "+----------- %d -----------+" (i + 1);
            string_of_thread_group_err err])
        |> List.concat |> String.concat "\n")

  exception ThreadGroupErrs of thread_group_err list

  let _ =
    Printexc.register_printer
      (function ThreadGroupErrs errs -> Some (string_of_thread_group_errs errs) | _ -> None)

  let thread_group_result_to_exn = function
    | Ok x -> x
    | Error errs -> raise (ThreadGroupErrs errs)

end

module CounterInstance () = struct
  let instance = Counter.create_counter ()

  (** Like {!Stdlib.Thread}, but with counted threads *)
  module Thread = struct
    include Thread
    let create f x = Counter.spawn_thread instance Util.(ignore % f) x
  end
end

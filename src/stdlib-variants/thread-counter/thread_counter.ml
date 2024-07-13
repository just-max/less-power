exception TooManyThreads of int
exception ThreadsLeftRunning of int * int
exception StopThread


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


let _ =
  Printexc.register_printer (function
    | TooManyThreads n -> Some (Printf.sprintf "Used too many threads (> %d)" n)
    | ThreadsLeftRunning (n, limit) ->
        Some
          (Printf.sprintf "Too many threads were left running (%d > %d)" n limit)
    | _ -> None)


module ThreadH = Hashtbl.Make (struct
  type t = Thread.t

  let equal t1 t2 = compare (Thread.id t1) (Thread.id t2) = 0
  let hash t = Hashtbl.hash (Thread.id t)
end)

type threadset = unit ThreadH.t


module Counter = struct

  open Common.Ctx_util

  type state = Running | Stopped | Overflowed

  type threadgroup = {
    running : threadset;
    overflow : Condition.t; (* predicate P(group) := group.state = Overflowed *)
    mutable state : state;
    max_threads : int;
    owner : t;
  }

  and t = { mut : Mutex.t; groups : threadgroup ThreadH.t }

  let create_thread ?group cnt f x =
    d_ "create_thread";

    let< _ = lock_mutex cnt.mut in

    let group =
      match group with
      | Some g -> g
      | None -> ThreadH.find cnt.groups (Thread.self ())
    in

    let make_thread () =
      let tid =
        Thread.create
          (fun () ->
            d_ "thread#%d started" Thread.(self () |> id);
            Fun.protect
              (fun () -> f x)
              ~finally:(fun () ->
                let< _ = lock_mutex cnt.mut in
                let tid = Thread.self () in
                ThreadH.remove group.running tid;
                ThreadH.remove cnt.groups tid;
                d_ "thread#%d finished" Thread.(self () |> id)))
          ()
      in
      ThreadH.replace cnt.groups tid group;
      ThreadH.replace group.running tid ();
      tid
    in

    match group.state with
    | Running ->
        if ThreadH.length group.running >= group.max_threads then (
          group.state <- Overflowed;
          Condition.broadcast group.overflow;
          (* Mutex.unlock cnt.mut; *)
          Thread.self ()
          (* raise? *)
          (* raise StopThread *))
        else make_thread ()
    | Overflowed | Stopped ->
        (* Mutex.unlock cnt.mut (* raise? *); *)
        Thread.self ()
  (* raise StopThread *)

  let create_counter () = { mut = Mutex.create (); groups = ThreadH.create 32 }

  let create_group ~max_threads cnt =
    d_ "create_group";
    (* no lock needed, since we don't write to the owner;
       operations on the group will lock instead *)
    {
      running = ThreadH.create 8;
      state = Running;
      overflow = Condition.create ();
      max_threads;
      owner = cnt;
    }

  let wait_for_overflow group =
    d_ "wait_for_overflow";
    let< _ = lock_mutex group.owner.mut in
    while not (group.state = Overflowed) do
      Condition.wait group.overflow group.owner.mut
    done

  let set_stopped group =
    d_ "set_stopped";
    let< _ = lock_mutex group.owner.mut in
    if group.state = Running then group.state <- Stopped

  let join ~timeout ?(allowed = 0) group =
    d_ "join";
    set_stopped group;
    let time0 = Mtime_clock.counter () in
    let rec task () =
      let remaining = ThreadH.length group.running in
      if remaining <= allowed then remaining
      else if Mtime.Span.compare (Mtime_clock.count time0) timeout > 0 then remaining
      else (
        Thread.yield ();
        task ())
    in
    task ()

  let run_counted ~max_threads ~max_leftover ~join_timeout counter f x =

    let result = ref None in
    let mut = Mutex.create () in
    let task_done = Condition.create () in

    let group = create_group ~max_threads counter in

    create_thread ~group counter
      (fun () ->
        let r = Some (try Ok (f x) with e -> Error e) in
        let< _ = lock_mutex mut in
        result := r;
        Condition.signal task_done)
      ()
    |> ignore;

    Thread.create
      (fun () ->
        wait_for_overflow group;
        let< _ = lock_mutex mut in
        Condition.signal task_done)
      ()
    |> ignore;
  
    let _ =
      let< _ = lock_mutex mut in
      while not (Option.is_some !result || group.state = Overflowed) do
        Condition.wait task_done mut
      done
    in
  
    let remaining = join ~allowed:max_leftover ~timeout:join_timeout group in
  
    let r =
      (match !result with
      | Some (Ok x) -> Ok x
      | Some (Error e) -> Error [e]
      | None -> Error [TooManyThreads group.max_threads])
    in
    if remaining > max_leftover then
      add_err (ThreadsLeftRunning (remaining, max_leftover)) r
    else r

end

module CounterInstance () = struct
  let instance = Counter.create_counter ()

  (** Like {!Stdlib.Thread}, but with counted threads *)
  module Thread = struct
    include Thread
    let create f x = Counter.create_thread instance f x
  end
end

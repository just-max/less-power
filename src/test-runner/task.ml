(** Setup, build, and run tests using first-class tasks.

    Tasks define actions to run when testing submissions. Examples include
    building student code, checking for forbidden syntax elements, or
    running the generated test executable.

    This module provides tasks as trees (type {!Tree.t}), so tasks can be
    more easily re-used and composed.

    Running tasks automatically handles exceptions and timeouts.
*)

(** Composable tasks, with exception and timeout handling. *)

open Common
open Common.Util

let f = Printf.sprintf

(** Check if the directory contains a symlink. The directory itself may be a symlink. *)
let contains_symlink p =
  let exception Symlink_found in
  Path_util.readdir_p p
  |> Seq.exists FileUtil.(
      fun p1 ->
        try find ~follow:Skip Is_link p1 (fun _ _ -> raise Symlink_found) false
        with Symlink_found -> true)

module Task_tree = struct
  (** Task trees represent a series of tasks to be run. *)

  exception Task_failure of (Format.formatter -> unit)

  (* let fail msg = raise (Task_failure (fun fmt -> Format.pp_print_string fmt msg)) *)
  let failf fmt = Format.kdprintf (fun pp -> raise (Task_failure pp)) fmt
  let fail msg = failf "%s" msg

  type task_failure =
    | Timed_out
    | Exception_raised of exn
    | Task_failed of (Format.formatter -> unit)

  let pp_task_failure fmt = function
    | Timed_out -> Format.fprintf fmt "%s" "task timed out"
    | Exception_raised exn -> Format.pp_print_string fmt @@ Printexc.to_string exn
    | Task_failed pp -> Format.fprintf fmt "%t" pp

  (** A list of tasks, which start by accepting a value of
      type ['a] and produce a result of type ['b] *)
  type (_, _) l =
    | Nil : ('a, 'a) l
    | Cons : ('a, 'b) t * ('b, 'c) l -> ('a, 'c) l

  (** Task continuation of type ['b k]: Upon completion, a task produces a
      value of some type ['a], and a list of subtasks from ['a] to ['b]. *)
  and _ k = Cont : 'a * ('a, 'b) l -> 'b k

  and ('a, 'b) t =
    {
      label : string option ;
      timeout : Mtime.span option ;
      task : 'a -> 'b k ;
    }
  (** A single task. *)

  let rec app : type a b c. (a, b) l -> (b, c) l -> (a, c) l = fun xs ys ->
    match xs with
    | Nil -> ys
    | Cons (x, xs') -> Cons (x, app xs' ys)

  let rec of_list = function [] -> Nil | t :: ts -> Cons (t, of_list ts)
  let singleton t = Cons (t, Nil)
  let snoc ts t = app ts (singleton t)
  let then_ t2 t1 = app (singleton t1) (singleton t2)

  type summary = {
    label : string option;
    elapsed_task : Mtime.span (** does not include subtasks *);
    elapsed_total : Mtime.span (** includes subtasks *);
    subtasks : summary list;
  }

  let rec get_last_task s =
    match List.rev s.subtasks with
    | [] -> ([], s)
    | s_last :: _ ->
        let path, last = get_last_task s_last in
        (s_last :: path, last)

  type 'a st_in = { x : 'a; timeout : Mtime.span option }

  (* this doesn't strictly have to be a record *)
  type 'a st_out = {
    x : ('a, task_failure) result
      (** If the last task completed successfully with a value of [x],
          then [Ok x]. Otherwise the failure reason of the task that failed. *);
    }

  let update_state_in x elapsed (st : _ st_in) =
    { x ; timeout = Option.map (Mtime.Span.abs_diff elapsed) st.timeout }

  (* TODO: check if elapsed is greater than timeout, then also consider as timeout *)

  (** Run a list of tasks. *)
  let rec run_list : type a b. a st_in -> (a, b) l -> b st_out * summary list =
    fun state -> function
    | Nil -> ({ x = Ok state.x }, [])
    | Cons (t, ts) ->
        let t_state, t_summary = run_task state t in
        match t_state.x with
        (* continue with the rest of the list *)
        | Ok x' ->
            let ts_state, ts_summary =
              run_list (update_state_in x' t_summary.elapsed_total state) ts
            in
            ({ x = ts_state.x; }, t_summary :: ts_summary)
        (* error occurred, stop running tree *)
        | Error _ as e -> ({ x = e }, [ t_summary ])

  (** Run a tree of tasks. *)
  and run_task : type a b. a st_in -> (a, b) t -> b st_out * summary =
    fun state t ->
    (* there could already be a timeout running,
       or this task has a timeout: take the smaller one *)
    let remaining_time = match state.timeout, t.timeout with
      | Some tm1, Some tm2 -> Some (min_span tm1 tm2)
      | Some tm, _ | _, Some tm -> Some tm
      | None, None -> None
    in

    let result, t_elapsed =
      let open Ctx_util in
      let< () = timed in
      let< () = capture_exceptions () in
      let< () = optional_timeout_unix ?timeout:remaining_time in
      t.task state.x
    in

    let make_result ?(subtasks_elapsed = Mtime.Span.zero) ?(subtasks_summary = []) x =
      let elapsed_total = Mtime.Span.add t_elapsed subtasks_elapsed in
      ( { x },
        { label = t.label; elapsed_task = t_elapsed;
          elapsed_total; subtasks = subtasks_summary } )
    in

    (* inner option: timeout; outer result: exception during task run *)
    match result with
    | Ok (Some (Cont (x', subtasks))) ->
        let subtasks_state, subtasks_summary =
          run_list (update_state_in x' t_elapsed state) subtasks
        in
        let subtasks_elapsed =
          subtasks_summary |> List.map (fun s -> s.elapsed_total)
          |> List.fold_left Mtime.Span.add Mtime.Span.zero
        in
        make_result subtasks_state.x ~subtasks_elapsed ~subtasks_summary
    | Ok None -> make_result (Error Timed_out)
    | Error e ->
        let err = match e with
        | Task_failure pp -> Task_failed pp
        | _ -> Exception_raised e
        in
        make_result (Error err)

  let run t x = run_task { x ; timeout = None } t

  let task1 ?timeout ?label task =
    { label; timeout; task = (fun x -> Cont (task x, Nil)) }

  let group ?timeout ?label subtasks =
    { label; timeout; task = (fun x -> Cont (x, subtasks)) }

  let with_ ?timeout ?label (t : _ t) =
    { t with label = or_option label t.label; timeout = or_option timeout t.timeout }

  type (_, _) unsnoc = Unsnoc : ('a, 'b) l * ('b, 'c) t -> ('a, 'c) unsnoc
  (** [Unsnoc ts lst], is the list [snoc ts lst], but split into [ts] and [lst].
      The list [ts] contains all elements of the list except the last one,
      and [lst] is the last element. *)

  (** [unsnoc t1 ts] splits the non-empty list [cons t1 ts] into its initial
      elements and final element, see {!type-unsnoc}. *)
  let rec unsnoc : type a b c. (a, b) t -> (b, c) l -> (a, c) unsnoc =
    fun t0 -> function
    | Nil -> Unsnoc (Nil, t0)
    | Cons (t1, ts) ->
        let Unsnoc (init, lst) = unsnoc t1 ts
        in Unsnoc (Cons (t0, init), lst)

  let rec then_map_k : type a b. (a -> b) -> a k -> b k =
    fun f -> function
    | Cont (x, Nil) -> Cont (f x, Nil)
    | Cont (x, Cons (t, ts)) ->
        let Unsnoc (init, lst) = unsnoc t ts in
        Cont (x, snoc init (then_map f lst))

  (** Modify the final result of a task tree, without creating an additional
      task. Note: since [f] runs inside the final task, its side-effects should
      be minimal: any exceptions will be raised inside an existing task, and the
      existing task's timeout will include [f]. If that is undesirable, run [f]
      inside a new subtask with {!then_subtask}. *)
  and then_map : type a b c. (b -> c) -> ((a, b) t) -> (a, c) t = fun f t ->
    { t with task = t.task %> then_map_k f }

  (** Ignore the result of a task (replace it with [()]). *)
  let ignore t = t |> then_map ignore

  let first_map f t = { t with task = fun x -> t.task (f x) }

  let then_subtask st t =
    let task x =
      let Cont (y, sts) = t.task x in
      Cont (y, app sts (singleton st))
    in
    { t with task }

  let accumulator f t =
    let task x =
      let Cont (y, ts') = t.task () in
      Cont (y, snoc ts' (task1 (f x)))
    in
    { t with task }

  let accumulating f z ts =
    let task _ = Cont (z, of_list (List.map (accumulator f) ts)) in
    { timeout = None; label = None; task }

  let collecting ts =
    accumulating (fun xs x -> x :: xs) [] ts |> then_map List.rev

  let pp_summary ~failure ?(show_anon = false) () : summary Fmt.t =
    let open Fmt in
    let f (s : summary) = show_anon || Option.is_some s.label in
    let rec pp_summary failure ppf (summary : summary) =
      pf ppf "@[<v 2>+ @[%a (%a)   %s@]%a@]"
        (option ~none:(const string "<anon>") string) summary.label
        Mtime.Span.pp summary.elapsed_total
        (if failure then "❌" else "✅")
        (pp_summaries failure) summary.subtasks
    and pp_summaries failure ppf (summaries : summary list) =
      pf ppf "%a"
        (option @@
          pair (list ~sep:nop @@ cut ++ pp_summary false) (pp_summary failure))
        (List.filter f summaries |> Util.unsnoc)
    in
    pp_summary failure

  let pp_state_out pp_x ppf (state : _ st_out) =
    let open Fmt in
    pf ppf "@[<v>Result:@,@[%a@]@]"
      (result ~ok:pp_x ~error:pp_task_failure) state.x

end

(** Composable tasks, with exception and timeout handling. *)

open Common
open Common.Util
open Common.Ctx_util

let f = Printf.sprintf

(* TODO: split Task_tree into Task.Tree and Task.List, with custom syntax (?) *)

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

module Messages = struct
  let submission_error =
    "Unable to build submission: \
     ensure that your code builds and matches the provided interface"
  let test_failure =
    "Error while running tests: \
     please report this failure to an instructor"
  let symlink = "Cannot build with symlinks present"
end

module Task = struct
  (** Common task components for running tests. *)

  open Task_tree

  type cfg = {
    build_root : FilePath.filename (** relative paths are OK *);
    safe : bool ;

    build_timeout : Mtime.span ;
    test_timeout : Mtime.span ;
    probe_timeout : Mtime.span ;

    exercise_start : float (** UTC time *);
    exercise_end : float (** UTC time *);
  }

  let check_contains_symlink cfg p =
    task1 ~label:(f"check_symlink[%s]" p) @@ fun _ ->
        if contains_symlink Path_util.(cfg.build_root / p)
        then fail Messages.symlink

  let write_file_pp ?label cfg pp p =
    task1 ?label @@ fun x ->
        let< ch = Out_channel.with_open_text Path_util.(cfg.build_root / p) in
        let ppf = Format.formatter_of_out_channel ch in
        pp ppf x;
        Format.pp_print_flush ppf ()

  let write_file_f ?label cfg p =
    Format.kdprintf @@ fun pp ->
        first_map (Fun.const pp) @@ write_file_pp ?label cfg (Fmt.fmt "%t") p

  let write_file_str ?label cfg s p =
    write_file_pp ?label cfg (fun fmt _ -> Format.pp_print_string fmt s) p

  let configure_show_hidden cfg p =
    let check =
      task1 @@ fun _ ->
          cfg.safe &&
          (let now = Unix.gettimeofday () in (* TODO: is this actually UTC? seems so... *)
          cfg.exercise_start <= now && now <= cfg.exercise_end)
    in
    let write =
      write_file_pp cfg
        (fun fmt -> Format.fprintf fmt "let show_hidden = %b")
        p
    in
    group ~label:"configure_show_hidden" (check |> then_ write)

  (** Task: remove the files which match [condition] (default: [.ml] files).
      Not recursive: only files in [p] will be deleted, not in subdirectories. *)
  let remove_files ?(condition = FileUtil.Has_extension "ml") cfg p =
    task1 (* ?label:(if_option label (f"remove_files[%s]" p)) *) @@ fun _ ->
        Path_util.readdir_p Path_util.(cfg.build_root / p)
        |> Seq.iter (fun p1 ->
            if Path_util.is_code ~condition p1 then Sys.remove p1)

  (** Task: copy the files which match [condition] (default: [.ml] files).
      Not recursive: only files in [src0] will be copied, not in subdirectories. *)
  let copy_files ?(condition = FileUtil.Has_extension "ml") cfg src0 dst0 =
    task1 (* ~label:(f"copy_code[%s->%s]" src0 dst0) *) @@ fun _ ->
        let src = Unix.realpath Path_util.(cfg.build_root / src0) in
        let dst = Path_util.(cfg.build_root / dst0) in
        Path_util.readdir_p src
        |> Seq.filter (Path_util.is_code ~condition)
        |> Seq.iter (fun src_p ->
            Unix.symlink src_p Path_util.(dst / Filename.basename src_p))

  (** Composition of {!remove_files} and {!load_files}. *)
  let load_files cfg ?condition src dst =
    let rm = remove_files ?condition cfg dst in
    let cp = (copy_files ?condition cfg src dst) in
    group (* ~label:(f"load_files[%s->%s]" src dst) *) (rm |> then_ cp)

  let make_test_report_directory cfg p =
    let mk () = Path_util.mkdir ~exist_ok:true Path_util.(cfg.build_root / p) in
    let rm = remove_files ~condition:(FileUtil.Has_extension "xml") cfg p in
    group (task1 mk |> then_ rm)

  (** Run the {{!module-Ast_check.val-path_violations}AST-checker} as a task. *)
  let checker cfg ?prohibited ?(limit = 3) ?check1 ?check p =
    task1 ~label:(f"checker[%s]" p) @@ fun _ ->
        let open Fmt in
        let buff = Buffer.create 512 in
        let ppf = Format.formatter_of_buffer buff in

        let violation = ref false in
        let k = function
          | [] -> ()
          | vs ->
              violation := true;
              pf ppf "%a" (vbox (list ~sep:cut (box Ast_check.pp_violation))) vs
        in
        Ast_check.path_violations
          ?prohibited ~limit ?check1 ?check k Path_util.(cfg.build_root / p);

        Format.pp_print_flush ppf ();
        if !violation then
          failf "%a" buffer buff

  (** Run a process as a task *)
  let subprocess_run ?timeout ?args command =
    task1 @@ fun _ -> P_run.p_run ?timeout ?args command

  (** Handle the result of a subprocess task. *)
  let subprocess_result ?hide_stdout ?hide_stderr
      ?command_line ?error_message ?check_status ?(dump_output = true) () =
    let open P_run in
    task1 @@ fun r ->
        let ok = P_run.result_is_ok r ?check_status in
        let message =
          Format.dprintf "@[<v>%a%a@]"
            (pp_result ?hide_stdout ?hide_stderr ?command_line) r
            Fmt.(option (cut ++ Pp_util.pp_text))
            (if ok then None else error_message)
        in
        (* TODO: dumping the output should probably happen directly in the
           subprocess runner, but since that is kept simple, it is currently
           not supported. *)
        if dump_output then
          Format.fprintf Format.err_formatter
            "@[<v 2>[INFO] subprocess complete:@,%t@]@."
            message;
        if ok then r else failf "%t" message

  type subprocess_options = {
    timeout : P_run.timeout_description option;
    hide_stdout : bool;
    hide_stderr : bool;
    error_message : string option;
    check_status : bool;
    dump_output : bool;
  }

  let subprocess_options ?timeout ?(hide_stdout = false) ?(hide_stderr = false)
      ?error_message ?(check_status = true) ?(dump_output = true) () =
    { timeout; hide_stdout; hide_stderr; error_message; check_status; dump_output }

  let subprocess cfg ?(options = subprocess_options ()) ?args command =
    let run = subprocess_run ?timeout:options.timeout ?args command in
    let result =
      subprocess_result ()
        ~hide_stdout:(options.hide_stdout && cfg.safe)
        ~hide_stderr:(options.hide_stderr && cfg.safe)
        ~command_line:(command :: Option.value args ~default:[])
        ?error_message:options.error_message
        ~check_status:options.check_status
        ~dump_output:(options.dump_output && not cfg.safe)
    in
    group (run |> then_ result)

  let dune cfg ?options ~root ?(args = []) cmd =
    subprocess cfg ?options "opam"
      ~args:(["exec"; "--"; "dune"; cmd; "--root"; Path_util.(cfg.build_root / root)] @ args)

  let timeout_for cfg = function
    | `Build -> cfg.build_timeout
    | `Test -> cfg.test_timeout
    | `Probe -> cfg.probe_timeout

end

(* export P_run *)
module P_run = P_run


(* TODO: move to entry_point *)

open Cmdliner

type 'a runner =
  build_root:FilePath.filename ->
  safe:bool ->
  build_timeout:Mtime.span ->
  test_timeout:Mtime.span ->
  probe_timeout:Mtime.span ->
  exercise_start:float ->
  exercise_end:float ->
  'a

let build_root =
  let doc = "Build and run tests relative to this directory." in
  Arg.(
    value
    & opt file Filename.current_dir_name
    & info ["d"; "build-root"] ~docv:"DIR" ~doc
  )

let safe =
  (* TODO: *may* be deleted, and currently aren't *)
  let doc =
    "Run tests in 'safe' mode. DANGER: template, solution, \
    and test repositories may be permanently deleted."
  in
  Arg.(value & flag & info ["s"; "safe"] ~doc)

let mtime_s =
  let open Arg in
  let parser =
    parser_of_kind_of_string
      ~kind:"a number"
      (fun s ->
        let float_s = Float.of_string_opt s in
        let float_ns = float_s |> Option.map (fun fs -> fs *. Mtime.Span.(to_float_ns s)) in
        Option.bind float_ns Mtime.Span.of_float_ns)
  in
  conv ~docv:"SECONDS" (parser, Mtime.Span.pp)

let mk_timeout ~names ~task ~default =
  let doc = f"Timeout used when %s, in seconds." task in
  Arg.(value & opt mtime_s default & info names ~doc)

let build_timeout =
  mk_timeout ~names:[ "build-timeout" ] ~default:Mtime.Span.(1 * min)
    ~task:"running `dune build` (or similar)"

let probe_timeout =
  mk_timeout ~names:[ "probe-timeout" ] ~default:Mtime.Span.(10 * s)
    ~task:"checking submission for long-running top-level code"

let test_timeout =
  mk_timeout ~names:[ "test-timeout" ] ~default:Mtime.Span.(10 * min)
    ~task:"running test executable"

(* TODO: timestamp formatting is ugly *)
let mk_timestamp ~default ~names ~when_ =
  let doc =
    f"%s time of the exercise (as a UNIX timestamp, in UTC). \
    Determines when secret test results are hidden and shown."
    (String.capitalize_ascii when_)
  in
  Arg.(value & opt float default & info names ~doc ~docv:"TIMESTAMP")

let exercise_start =
  mk_timestamp ~default:(-. max_float)
    ~names:[ "exercise-start" ] ~when_:"start"

let exercise_end =
  mk_timestamp ~default:max_float ~names:[ "exercise-end" ] ~when_:"end"


let term_of_runner runner =
  Term.(
    const runner $ build_root $ safe $ build_timeout $ probe_timeout
      $ test_timeout $ exercise_start $ exercise_end
  )

let runner_with_cfg of_cfg build_root safe build_timeout probe_timeout
    test_timeout exercise_start exercise_end =
  Task.{
    build_root; safe; build_timeout; probe_timeout;
    test_timeout; exercise_start; exercise_end
  }
  |> of_cfg

let task_runner task_of_cfg cfg =
  let open Task_tree in
  let task : (unit, unit) t = task_of_cfg cfg in
  let result, summary = run task () in
  Format.printf "@[<v>Task summary:@,%a@,%a@]@."
    (Task_tree.pp_summary ~failure:(Result.is_error result.x) ()) summary
    (Task_tree.pp_state_out Fmt.(const string "Build successful.")) result

let command_of_term term =
  let doc = "test runner" in
  let info = Cmd.info "test-runner" ~doc in
  Cmd.v info term

let run_task_main ?(exit = exit) task_of_cfg =
  let term = term_of_runner (runner_with_cfg (task_runner task_of_cfg)) in
  let cmd = command_of_term term in
  Cmd.eval cmd |> exit

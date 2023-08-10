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
    | Timed_out of Mtime.span
    | Exception_raised of exn
    | Task_failed of (Format.formatter -> unit)

  let pp_task_failure fmt = function
    | Timed_out time -> Format.fprintf fmt "task timed out after %a" Mtime.Span.pp time
    | Exception_raised exn -> Format.pp_print_string fmt @@ Printexc.to_string exn
    | Task_failed pp -> Format.fprintf fmt "%t" pp

  type ctx = { label : string option } (* timeout and elapsed time? *)
  type ctxs = ctx list

  (** A list of tasks, which start by accepting a value of
      type ['a] and produce a result of type ['b] *)
  type ('a, 'b) l =
    | Nil : ('a, 'a) l
    | Cons : ('a, 'b) t * ('b, 'c) l -> ('a, 'c) l

  (** Task continuation of type ['b k]: Upon completion, a task produces a
      value of some type ['a], and a list of subtasks from ['a] to ['b]. *)
  and 'a k = Cont : 'a * ('a, 'b) l -> 'b k

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

  type 'a st_in = { x : 'a ; timeout : Mtime.span option ; parents : ctxs }

  type 'a st_out = {
    x : ('a, ctxs * task_failure) result ;
      (** If the last task completed successfully with a value of [x], then [Ok x].
          Otherwise the context of the task that failed and the failure reason. *)
    passed : ctxs list ; (** The tasks that completed successfully *)
    elapsed : Mtime.span ; (** Elapsed time. *)
  }

  let update_state_in x elapsed (st : _ st_in) =
    { st with x ; timeout = Option.map (Mtime.Span.abs_diff elapsed) st.timeout }
  let merge_state_out st1 st2 =
    { st2 with passed = st1.passed @ st2.passed ; elapsed = Mtime.Span.add st1.elapsed st2.elapsed }

  (** Run a list of tasks. The result is as per {!st_out}. *)
  let rec run_list : type a b. a st_in -> (a, b) l -> b st_out = fun st ->
    function
    | Nil -> { x = Ok st.x ; passed = [] ; elapsed = Mtime.Span.zero }
    | Cons (t, ts) ->
        let st_t = run_task st t in
        match st_t.x with
        | Ok x' ->
            let st_ts = run_list (update_state_in x' st_t.elapsed st) ts in
            merge_state_out st_t st_ts
        | Error _ as e -> { st_t with x = e }

  (** Run a tree of tasks. The result is as per {!st_out}. *)
  and run_task : type a b. a st_in -> (a, b) t -> b st_out = fun st t ->
    let t_id = { label = t.label } :: st.parents in
    (* there could already be a timeout running, or this task has a timeout: take the smaller one *)
    let remaining_time = match st.timeout, t.timeout with
      | Some tm1, Some tm2 -> Some (min_span tm1 tm2)
      | Some tm, _ | _, Some tm -> Some tm
      | None, None -> None
    in
    let run_timeout =
      let f () = t.task st.x in (* todo: check if elapsed is greater than timeout, then also consider as timeout *)
      match remaining_time with
      | Some tm -> Option.to_result ~none:tm % Util.timeout_unix tm f
      | None -> Result.ok % f
    in
    let r, t_elapsed = Util.timed (Util.try_to_result run_timeout) () in
    (* inner Result: timeout; outer Result: exception during task run *)
    match r with
    | Ok (Error tm) -> { x = Error (t_id, Timed_out tm) ; passed = [] ; elapsed = t_elapsed }
    | Ok (Ok (Cont (x', subtasks))) ->
        let st_ts = run_list { (update_state_in x' t_elapsed st) with parents = t_id } subtasks in
        merge_state_out { x = Ok x' ; passed = [t_id] ; elapsed = t_elapsed } st_ts (* TODO: put t_id to the end of the passed test list, only if all subtasks pass *)
    | Error e ->
        let err = match e with
        | Task_failure pp -> Task_failed pp
        | _ -> Exception_raised e
        in
        { x = Error (t_id, err) ; passed = [] ; elapsed = t_elapsed }

  let run t x = run_task { x ; timeout = None ; parents = [] } t

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

  (* let pp_ctx fmt ctx = Format.pp_print_string fmt ctx.label *)

  (* TODO: task label formatting is meh with respect to Some/None *)
  let pp_ctxs fmt : ctxs -> unit =
    let open Format in
    List.filter_map (fun (ctx : ctx) -> ctx.label) %>
    List.rev %>
    fprintf fmt "@[<hv 2>%a@]"
      (pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt "@,.")
        pp_print_string)

  let pp_state_out pp_x fmt st =
    let open Format in
    fprintf fmt "@[<v>@[<v>%a@]@,%a@,@[%a@]@]"
      (pp_print_list (fun fmt -> fprintf fmt "%a: OK" pp_ctxs))
      (List.filter (function ({ label = Some _ ; _ } : ctx) :: _ -> true | _ -> false) st.passed)
      (fun fmt -> fprintf fmt "@[After %a:@]" Mtime.Span.pp) st.elapsed
      (fun fmt -> function
        | Ok x -> pp_x fmt x
        | Error (task, e) ->
            fprintf fmt "@[%a: FAIL@]@ @[%a@]" pp_ctxs task pp_task_failure e)
      st.x

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

    hidden_start : float (** UTC time *);
    hidden_end : float (** UTC time *);
  }

  let check_contains_symlink cfg p =
    task1 ~label:(f"check_symlink[%s]" p) @@ fun _ ->
        if contains_symlink Path_util.(cfg.build_root / p)
        then fail Messages.symlink

  let write_file_pp ?label cfg pp p =
    task1 ?label @@ fun x ->
        Out_channel.with_open_text Path_util.(cfg.build_root / p) @@ fun ch ->
            let ppf = Format.formatter_of_out_channel ch in
            pp ppf x; Format.pp_print_flush ppf ()

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
          cfg.hidden_start <= now && now <= cfg.hidden_end)
    in
    let write =
      write_file_pp cfg
        (fun fmt -> Format.fprintf fmt "let show_hidden = %b")
        p
    in
    group ~label:"configure_show_hidden" (check |> then_ write)

  (** Task: remove the files which match [condition] (default: [.ml] files).
      Not recursive: only files in [p] will be deleted, not in subdirectories. *)
  let remove_code1 ?(condition = FileUtil.Has_extension "ml") cfg p =
    task1 ~label:(f"remove_code[%s]" p) @@ fun _ ->
        Path_util.readdir_p Path_util.(cfg.build_root / p)
        |> Seq.iter (fun p1 ->
            if Path_util.is_code ~condition p1 then Sys.remove p1)

  (** Task: copy the files which match [condition] (default: [.ml] files).
      Not recursive: only files in [src0] will be copied, not in subdirectories. *)
  let copy_code1 ?(condition = FileUtil.Has_extension "ml") cfg src0 dst0 =
    task1 ~label:(f"copy_code[%s->%s]" src0 dst0) @@ fun _ ->
        let src = Unix.realpath Path_util.(cfg.build_root / src0) in
        let dst = Path_util.(cfg.build_root / dst0) in
        Path_util.readdir_p src
        |> Seq.filter (Path_util.is_code ~condition)
        |> Seq.iter (fun src_p ->
            Unix.symlink src_p Path_util.(dst / Filename.basename src_p))

  (** Composition of {!remove_code1} and {!load_code1}. *)
  let load_code1 cfg ?condition src dst =
    let rm = remove_code1 ?condition cfg dst in
    let cp = (copy_code1 ?condition cfg src dst) in
    group ~label:(f"load_code[%s->%s]" src dst) (rm |> then_ cp)

  (** Run the {{!module-Ast_check.val-path_violations}AST-checker} as a task. *)
  let checker cfg ?prohibited ?(limit = 3) ?check1 ?check p =
    task1 ~label:(f"checker[%s]" p) @@ fun _ ->
        let open Fmt in
        let buff = Buffer.create 512 in
        let ppf = Format.formatter_of_buffer buff in
        (* Format.pp_open_vbox ppf 0; *)

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
      ?command_line ?error_message ?(check_status = true) () =
    let open P_run in
    task1 @@ fun r ->
        match[@warning "-4"] r.phase, r.status with
          | Completed, _ when not check_status -> r
          | Completed, Unix.WEXITED 0 -> r
          | _ ->
            failf "@[<v>%a%a@]"
              (pp_result ?hide_stdout ?hide_stderr ?command_line) r
              Fmt.(option (cut ++ Pp_util.pp_text)) error_message

  type subprocess_options = {
    timeout : P_run.timeout_description option;
    hide_stdout : bool;
    hide_stderr : bool;
    error_message : string option;
    check_status : bool;
  }

  let subprocess_options ?timeout ?(hide_stdout = false) ?(hide_stderr = false)
      ?error_message ?(check_status = true)() =
    { timeout; hide_stdout; hide_stderr; error_message; check_status; (* command; args; *) }

  let subprocess cfg ?(options = subprocess_options ()) ?args command =
    let run = subprocess_run ?timeout:options.timeout ?args command in
    let result =
      subprocess_result ()
        ~hide_stdout:(options.hide_stdout && cfg.safe)
        ~hide_stderr:(options.hide_stderr && cfg.safe)
        ~command_line:(command :: Option.value args ~default:[])
        ?error_message:options.error_message
        ~check_status:options.check_status
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

let std_build p =
  let open Task_tree in
  let open Task in
  let cfg = {
    build_root = p ;
    safe = false ;

    build_timeout = Mtime.Span.(1 * min);
    test_timeout = Mtime.Span.(10 * min) ;
    probe_timeout = Mtime.Span.(10 * s) ;

    hidden_start = 0. ;
    hidden_end = Float.max_float ;
  }
  in
  let t_setup = group ~label:"setup" @@ of_list [
    (* check student code for symlinks *)
    check_contains_symlink cfg "assignment" ;
    (* load code from the student repository *)
    load_code1 cfg "assignment/src" "tests/assignment" ;

    (* load the sample solution *)
    load_code1 cfg "solution/src" "tests/solution" ;
    write_file_str cfg "include Assignment\n" "tests/solution/solution.ml" ;

    (* configure the tests *)
    configure_show_hidden cfg "tests/test/config.ml" ;
  ]
  in
  (* run the limitation checker *)
  let t_check = checker cfg "tests/assignment" in
  (* build the test binary *)
  let t_build = group ~label:"build" @@ of_list [
    (* First, build only the student submission without referencing the tests
       or the solution, so that we can show the build output to the student
       and not leak test or solution code. *)
    dune cfg
      ~options:(
        subprocess_options ()
          ~timeout:(P_run.timeout (timeout_for cfg `Build))
          ~error_message:Messages.test_failure)
      ~root:"tests/" "build" ~args:["--force"; "assignment"]
      |> ignore |> with_ ~label:"assignment";

    (* If there are build failures, the compiler sometimes prints source code
       of the tests or the solution to stderr, which is shown to the student.
       Therefore, drop output. If the student submission builds and matches the
       interface, this should never fail (and any failures are almost certainly
       our fault). *)
    dune cfg
      ~options:(
        subprocess_options ()
          ~timeout:(P_run.timeout (timeout_for cfg `Build))
          ~hide_stdout:true ~hide_stderr:true
          ~error_message:Messages.submission_error)
      ~root:"tests/" "build" ~args:["--force"; "test"]
      |> ignore |> with_ ~label:"test";
    ]
  in
  group ~label:"test" @@ of_list [t_setup; t_check; t_build]

(* next steps: actually execute the tests. almost there! *)

(* TODO:
   basic executable to bootstrap tests within the repository, i.e. which
   1. builds, e.g., tests/runner
   2. executes the resulting executable, possibly all in one step with dune exec
   
   Could also just be in bash... *)

(** Common tasks and tasks for the standard test setup. *)

open Task
open Common
open Ctx_util

let f = Printf.sprintf

(** {1 Common tasks} *)

(** Check if the directory contains a symlink. The directory itself may be a symlink. *)
let contains_symlink p =
  let exception Symlink_found in
  Path_util.readdir_p p
  |> Seq.exists FileUtil.(
      fun p1 ->
        try find ~follow:Skip Is_link p1 (fun _ _ -> raise Symlink_found) false
        with Symlink_found -> true)

module Messages = struct
  let submission_error =
    "Unable to build submission: \
     ensure that your code builds and matches the provided interface"
  let test_failure =
    "Error while running tests: \
     please report this failure to an instructor"
  let symlink = "Cannot build with symlinks present"
end

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
        let now = Unix.gettimeofday () in
        not cfg.safe || now < cfg.exercise_start || cfg.exercise_end < now
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


(** {1 Standard tasks} *)

let std_test_report_dir = "test-reports/"

(** Set up the test environment. *)
let std_setup cfg = group ~label:"setup" @@ of_list [
    group ~label:"load_submission" @@ of_list [
      (* check student code for symlinks *)
      check_contains_symlink cfg "assignment" ;
      (* load code from the student repository *)
      load_files cfg "assignment/src" "tests/assignment" ;
    ];

    (* load the sample solution *)
    group ~label:"load_solution" @@ of_list [
      load_files cfg "solution/src" "tests/solution" ;
      write_file_str cfg "include Assignment\n" "tests/solution/solution.ml" ;
    ];

    (* configure the tests *)
    configure_show_hidden cfg "tests/test/config.ml" ;

    (* make sure the directory exists and clear out old test reports *)
    make_test_report_directory cfg std_test_report_dir;
  ]

(** Run the limitation checker. *)
let std_check cfg = checker cfg "tests/assignment"

(* Build the assignment library and test binary. *)
let std_build cfg = group ~label:"build" @@ of_list [
  (* First, build only the student submission without referencing the tests
      or the solution, so that we can show the build output to the student
      and not leak test or solution code. *)
  dune cfg
    ~options:(
      subprocess_options ()
        ~timeout:(P_run.timeout (timeout_for cfg `Build))
        ~error_message:Messages.submission_error)
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
        ~error_message:Messages.test_failure)
    ~root:"tests/" "build" ~args:["--force"; "test"]
  |> ignore |> with_ ~label:"test";
  ]

(** Run the generated binary. *)
let std_test cfg = group ~label:"test" @@ of_list [
  (* The test executable is configured to exit immediately if no arguments are passed.
      We use this to check that the top level code of the submission does not
      contain any long-running code. If it does, there's no point in running the tests,
      since the significantly longer test timeout will just kill them.
      We don't want to prohibit top-level code entirely, since it would prevent
      things like [let impl0 = impl 0] *)
  dune cfg
    ~options:(
      subprocess_options ()
        ~timeout:(P_run.timeout (timeout_for cfg `Probe)))
    ~root:"tests/" "exec" ~args:["--no-build"; "--"; "test/test.exe"]
  |> ignore |> with_ ~label:"probe";

  (* run the test! *)
  dune cfg
    ~options:(
      subprocess_options ()
        ~timeout:(P_run.timeout (timeout_for cfg `Test)))
    ~root:"tests/" "exec"
    ~args:[
      "--no-build"; "--"; "test/test.exe";
      "-output-junit-file"; "test-reports/results.xml"]
  |> ignore |> with_ ~label:"run";
]

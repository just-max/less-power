(** Common tasks and tasks for the standard test setup. *)

open Task
open Common
open Ctx_util
open Util

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

  timestamp_now : float option (** Override current time, UTC *);
  exercise_start : float (** UTC time *);
  exercise_end : float (** UTC time *);
}

let check_contains_symlink cfg p =
  task1 ~label:(f"check_symlink[%s]" p) @@ fun _ ->
      if contains_symlink Path_util.(cfg.build_root / p)
      then fail Messages.symlink

let write_file_pp ?label cfg p pp =
  task1 ?label @@ fun x ->
      let< ch = Out_channel.with_open_text Path_util.(cfg.build_root / p) in
      let ppf = Format.formatter_of_out_channel ch in
      pp ppf x;
      Format.pp_print_flush ppf ()

let write_file_f ?label cfg p =
  Format.kdprintf @@ fun pp ->
      first_map (Fun.const pp) @@ write_file_pp ?label cfg p (Fmt.fmt "%t")

let write_file_str ?label cfg s p =
  write_file_pp ?label cfg p (fun fmt _ -> Format.pp_print_string fmt s)

let configure_show_hidden cfg p =
  let check =
    task1 @@ fun _ ->
        let now =
          match cfg.timestamp_now with
          | None -> Unix.gettimeofday ()
          | Some t -> t
        in
        not cfg.safe || now < cfg.exercise_start || cfg.exercise_end < now
  in
  let write =
    write_file_pp cfg p
      (fun fmt -> Format.fprintf fmt "let show_hidden = %b")
  in
  group ~label:"configure_show_hidden" (check |> then_ write)

(** Task: remove the files which match [condition] (default: [.ml] files).
    Not recursive: only files in [p] will be deleted, not in subdirectories. *)
let remove_files ?(condition = FileUtil.Has_extension "ml") cfg p =
  task1 @@ fun _ ->
      Path_util.readdir_p Path_util.(cfg.build_root / p)
      |> Seq.iter (fun p1 ->
          if Path_util.is_code ~condition p1 then Sys.remove p1)

(** Task: copy the files which match [condition] (default: [.ml] files).
    Not recursive: only files in [src0] will be copied, not in subdirectories. *)
let copy_files ?(condition = FileUtil.Has_extension "ml") cfg src0 dst0 =
  task1 @@ fun _ ->
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

let make_clean_directory ?(remove = FileUtil.True) cfg p =
  let mk _ = Path_util.mkdir ~exist_ok:true Path_util.(cfg.build_root / p) in
  let rm = remove_files ~condition:remove cfg p in
  group (task1 mk |> then_ rm)

let make_test_report_directory =
  make_clean_directory ~remove:(FileUtil.Has_extension "xml")

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
let subprocess_run ?timeout ?args ?env command =
  task1 @@ fun _ -> P_run.p_run ?timeout ?args ?env command

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

let subprocess cfg ?(options = subprocess_options ()) ?args ?env command =
  let run = subprocess_run ?timeout:options.timeout ?args ?env command in
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

let dune cfg ?options ~root ?(args = []) ?env cmd =
  subprocess cfg ?options "opam" ?env
    ~args:([
        "exec"; "--"; "dune"; cmd;
        "--no-print-directory";
        "--root"; Path_util.(cfg.build_root / root);
      ] @ args)

let timeout_for cfg = function
  | `Build -> cfg.build_timeout
  | `Test -> cfg.test_timeout
  | `Probe -> cfg.probe_timeout


(** {1 Standard tasks}

    These are high-level tasks built upon the primitives defined above.
    They work with the recommended layout of tests, submission and sample
    solution. However, the low-level tasks defined above can also be used
    as needed. *)

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

(** Build something in the [tests/] directory with dune.
    If [submission] is [false] (default), error output is hidden. *)
let std_build1 cfg ?(submission = false) ?env what =
  let options =
    subprocess_options ()
      ~timeout:(P_run.timeout (timeout_for cfg `Build))
      ~hide_stdout:(not submission) ~hide_stderr:(not submission)
      ~error_message:Messages.(
        if submission then submission_error else test_failure)
  in
  dune cfg ~options ~root:"tests/"
    "build" ~args:["--force"; what] ?env
  |> ignore

(** Run something in the [tests/] directory with [dune exec].

    This task passes the [--no-build] flag to dune. As such:
    - The executable must be built before
      running this task (e.g. with {!std_build1}).
    - There is no risk of showing the build output to the student.

    If [phase] is given, sets a timeout. *)
let std_exec1 cfg ?phase ?(args = []) ?env what =
  let options =
    subprocess_options ()
      ?timeout:(phase |> Option.map (timeout_for cfg %> P_run.timeout))
  in
  dune cfg ~options ~root:"tests/"
    "exec" ~args:(["--no-build"; "--"; what] @ args) ?env
  |> ignore

(** Build the assignment library and test binary. *)
let std_build cfg = group ~label:"build" @@ of_list [
  (* First, build only the student submission without referencing the tests
      or the solution, so that we can show the build output to the student
      and not leak test or solution code. *)
  std_build1 cfg ~submission:true "assignment" |> with_ ~label:"assignment";

  (* If there are build failures, the compiler sometimes prints source code
      of the tests or the solution to stderr, which is shown to the student.
      Therefore, drop output. If the student submission builds and matches the
      interface, this should never fail (and any failures are almost certainly
      our fault). *)
  std_build1 cfg ~submission:false "test" |> with_ ~label:"test";
]

(** Run a test executable in the standard setup. See the note about building
    first in {!std_exec1}. Assumes the test executable is configured to exit
    immediately if no command line arguments are passed.
    We use this to check that the top level code of the submission does not
    contain any long-running code. If it does, there's no point in running the
    tests, since the significantly longer test timeout will just kill them.
    We don't want to prohibit top-level code entirely, since it would prevent
    things like [let impl0 = impl 0].

    Thus, first runs the given executable without any arguments, then runs it a
    second time, with the given [output_junit_file] (interpreted relative to the
    [test-reports/] directory) as the output JUnit file. *)
let std_exec_test cfg ?env ?(shards = Some 1) ~output_junit_file what = group @@ of_list [
  (* Note: the 'probe' name here is unrelated to the partial signature
     checking probe below, and refers to the top-level check *)
  std_exec1 cfg ~phase:`Probe ?env what |> with_ ~label:"top_level";

  (* run the test! *)
  let args =
    (match shards with
    | Some i -> ["-shards"; string_of_int i]
    | None -> [])
    @
    [ "-output-junit-file";
      Path_util.(std_test_report_dir / output_junit_file); ]
  in
  std_exec1 cfg ~phase:`Test ?env what ~args |> with_ ~label:"run";
]

let std_output_junit_file = "results.xml"
let std_grading_junit_file = "grading.xml"

(** As {!std_exec_test}, fixed to [test/test.exe]
    with output to [test-reports/results.xml]. *)
let std_test ?shards cfg =
  std_exec_test cfg ?shards "test/test.exe"
    ~output_junit_file:std_output_junit_file
  |> with_ ~label:"test"

(** Run an executable that processes the test results in the standard setup.
    Executes [test/process_results.exe]. See the note about
    building first in {!std_exec1}.

    The given file names are taken relative to the [test-reports/]
    directory and passed to the executable. *)
let std_exec_process_results junit_files cfg =
  let args =
    junit_files
    |> List.map Path_util.(fun file -> std_test_report_dir / file)
  in
  std_exec1 cfg "test/process_results.exe" ~args
  |> with_ ~label:"process_results"

(** As {!std_exec_process_results}, but: if [grading] is [true], the first
    argument to the executable is [test-reports/grading.xml]. In any case,
    the remaining [output_junit_files] (taken relative to the [test-reports/]
    directory) follow, which defaults to [["results.xml"]]. *)
let std_process_results
    ?(grading = false) ?(output_junit_files = [std_output_junit_file]) cfg =
  let junit_files =
    (if grading then [std_grading_junit_file] else []) @ output_junit_files
  in
  std_exec_process_results junit_files cfg

(** {1 Probe-related tasks (partial type correctness checking)} *)

type probe_result = {
  name : string (** the name given to the probe definition *);
  ok : bool (** did the check pass? *);
  message : string (** compiler output *);
}

let probe_result_of_prun_result ~name p_run_result =
  P_run.{
    name;
    ok = result_is_ok ~check_status:true p_run_result;
    message =
      [ p_run_result.stdout; p_run_result.stderr ]
      |> List.map String.trim |> List.filter (fun s -> s <> "")
      |> String.concat "\n";
  }

let defines_of_probe_result probe_result =
  (if probe_result.ok then [f"%s_PROBE" probe_result.name] else []) @
  [
    f"%s_OK %b" probe_result.name probe_result.ok;
    f"%s_MSG %S" probe_result.name probe_result.message;
  ]

(** [[A-Z_]*] *)
let is_probe_name =
  let ok_chars =
    Hashtbl.of_seq @@ Seq.map (fun c -> c, ())
    @@ String.to_seq "_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  in
  fun s -> Seq.for_all (Hashtbl.mem ok_chars) (String.to_seq s)

(** parse the probe items from the given path *)
let load_probe_items cfg ~defs = task1 @@ fun _ ->
  let open Parsetree in
  let ast =
    Pparse.parse_implementation
      ~tool_name:"probe" Path_util.(cfg.build_root / defs)
  in
  let map_stri =
    let open Asttypes in
    function[@warning "-4"]
    | Pstr_module {
        pmb_name = { txt = Some name; _ };
        pmb_expr = { pmod_desc = Pmod_structure st ; _ };
        _
      } when is_probe_name name -> Some (name, st)
    | _ -> None
  in
  List.map (fun si -> si.pstr_desc) ast
  |> List.filter_map map_stri

let probe_dune ~name ~dep =
  Format.dprintf
    "(library (name %s)\
      (flags (:standard -w -a))\
      (libraries probe_common %s))"
    name dep

let probe_dir_path data_dir ~name = Path_util.(data_dir / name)

let gen_probe cfg ~data_dir ~dep ~name stri =
  let open Path_util in
  let probe_dir = probe_dir_path data_dir ~name in
  group @@ of_list [
    make_clean_directory cfg probe_dir
      ~remove:FileUtil.(Or (Basename_is "dune", Has_extension "ml"));
    write_file_f cfg (probe_dir / "dune") "%t" (probe_dune ~name ~dep);
    write_file_f cfg (probe_dir / "probe.ml") "%a" Pprintast.structure stri;
  ]

(** unlike the other tasks, [data_dir] needs to be relative to [root] here *)
let run_probe cfg ~root ~data_dir ~name =
  let probe_dir = probe_dir_path data_dir ~name in
  let probe () =
    dune cfg
      ~options:(
        subprocess_options ()
          ~check_status:false
          ~hide_stdout:true ~hide_stderr:true
          ~timeout:(P_run.timeout (timeout_for cfg `Build)))
      ~root "build" ~args:["--force"; probe_dir]
  in
  let mk_result = task1 (probe_result_of_prun_result ~name) in
  group (probe () |> then_ mk_result)

let handle_probe_results cfg ~args_file =
  first_map (List.concat_map defines_of_probe_result) @@
  let open Fmt in
  write_file_pp cfg args_file (vbox @@ list (const string "-D" ++ cut ++ string))

(** all paths are relative to [root] *)
let probe cfg ~root ~defs ~dep ~data_dir ~args_file =
  let root_rel d = Path_util.(root / d) in
  let mk_data_dir = make_clean_directory cfg (root_rel data_dir) in
  let load = load_probe_items cfg ~defs:(root_rel defs) in
  let probe1 (name, stri) =
    gen_probe cfg ~data_dir:(root_rel data_dir) ~dep ~name stri
    |> then_ @@ run_probe cfg ~root ~data_dir ~name
    |> group ~label:name
  in
  let probe_all =
    spawning (fun ps -> List.map probe1 ps |> collecting')
    |> with_ ~label:"items"
  in
  let post = handle_probe_results cfg ~args_file:(root_rel args_file) in
  mk_data_dir @:> load @:> probe_all @:> post @:> nil |> group ~label:"probe"

let std_probe cfg =
  probe cfg
    ~root:"tests/"
    ~defs:"probe/probe_defs.ml"
    ~dep:"assignment"
    ~data_dir:"probe-data/"
    ~args_file:".probe-args"

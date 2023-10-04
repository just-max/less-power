(** Entry points for creating test driver executables from a task tree. *)

open Common
open Cmdliner
open Std_task

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

let mk_timestamp ~names ?doc ?absent arg =
  Arg.(value & arg & info names ?doc ?absent ~docv:"TIMESTAMP")

let timestamp_now =
  let doc =
    "Use the given timestamp instead of the system time to \
      decide whether to show or hide hidden and secret tests."
  in
  Arg.(
    mk_timestamp ~names:[ "now" ] ~doc ~absent:"current time"
    & opt (some float) None)

let mk_timestamp_start_end ~when_ ~names ~default =
  let doc =
    f"%s time of the exercise (as a UNIX timestamp, in UTC). \
    Determines when secret test results are hidden and shown."
    (String.capitalize_ascii when_)
  in
  Arg.(mk_timestamp ~names ~doc ~absent:"unbounded" & opt float default)

let exercise_start =
  mk_timestamp_start_end ~default:Float.neg_infinity
    ~names:[ "exercise-start" ] ~when_:"start"

let exercise_end =
  mk_timestamp_start_end ~default:Float.infinity
    ~names:[ "exercise-end" ] ~when_:"end"

let term_of_runner runner =
  let runner0 build_root safe build_timeout probe_timeout
      test_timeout timestamp_now exercise_start exercise_end =
    if not (exercise_start < exercise_end) then
      `Error (true, "exercise start must be before exercise end")
    else
      `Ok
        (runner {
          build_root; safe; build_timeout; probe_timeout;
          timestamp_now; test_timeout; exercise_start; exercise_end
        })
  in
  Term.(
    ret
      (const runner0 $ build_root $ safe $ build_timeout $ probe_timeout
        $ test_timeout $ timestamp_now $ exercise_start $ exercise_end)
  )

(** [Testcase.error] if [message] is [Some _], otherwise [Testcase.pass] *)
let build_report_junit ?(time = Mtime.Span.zero) message =
  let open Junit in
  let test_case =
    let mk_testcase =
      match message with
      | None -> Testcase.pass
      | Some msg -> Testcase.error ~typ:"" msg
    in
    mk_testcase
      ~name:"build" ~classname:"build"
      ~time:(time |> Util.span_to_float_s)
  in
  let test_suite =
    Testsuite.make ~name:"build" ()
    |> Testsuite.add_testcases [test_case]
  in
  make [test_suite]

(** Creates a task from the config, and runs it. Reports the result as a
    test case (in [build_report_path], default [test-reports/build.xml]).
    If [report_success_details] is [false] (default), no detailed build
    report is shown when the build succeeds. Otherwise, the detailed report is
    always shown, which requires that the test case is marked as a failure.*)
let task_runner
    ?(build_report_path = Path_util.(std_test_report_dir / "build.xml"))
    ?(report_success_details = false)
    task_of_cfg (cfg : cfg) =
  let open Task in
  let task : (unit, unit) tree = task_of_cfg cfg in
  let result, summary = run task () in
  let pp_result =
    Format.dprintf "@[<v>Task summary:@,%a@,%a@]"
      (pp_summary ~failure:(Result.is_error result) ()) summary
      (pp_state_out Fmt.(const string "Build successful.")) result
  in

  Format.eprintf "%t@." pp_result;

  Path_util.mkdir (Filename.dirname build_report_path)
    ~exist_ok:true ~parents:true;
  let junit =
    build_report_junit
      (if Result.is_error result || report_success_details
        then Some (Format.asprintf "%t" pp_result) else None)
      ~time:summary.elapsed_total
  in
  Junit.to_file junit build_report_path

let command_of_term term =
  let doc = "test runner" in
  let info = Cmd.info "test-runner" ~doc in
  Cmd.v info term

let run_task_main ?build_report_path ?report_success_details
    ?(exit = exit) task_of_cfg =
  task_runner ?build_report_path ?report_success_details task_of_cfg
  |> term_of_runner |> command_of_term
  |> Cmd.eval |> exit

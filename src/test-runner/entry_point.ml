(** Entry points for creating test driver executables from a task tree. *)

open Cmdliner
open Std_task

(* TODO: remove *)
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
  {
    build_root; safe; build_timeout; probe_timeout;
    test_timeout; exercise_start; exercise_end
  }
  |> of_cfg

let task_runner task_of_cfg cfg =
  let open Task.Task_tree in
  let task : (unit, unit) t = task_of_cfg cfg in
  let result, summary = run task () in
  Format.printf "@[<v>Task summary:@,%a@,%a@]@."
    (pp_summary ~failure:(Result.is_error result.x) ()) summary
    (pp_state_out Fmt.(const string "Build successful.")) result

let command_of_term term =
  let doc = "test runner" in
  let info = Cmd.info "test-runner" ~doc in
  Cmd.v info term

let run_task_main ?(exit = exit) task_of_cfg =
  let term = term_of_runner (runner_with_cfg (task_runner task_of_cfg)) in
  let cmd = command_of_term term in
  Cmd.eval cmd |> exit

open Test_runner

let main _cmd args =
  let build_root = match args with [] -> "." | a :: _ -> a in
  let open Task_tree in
  run (std_build build_root) ()
  |> Format.printf "%a@." (pp_state_out Fmt.(const string "Build successful."));
  0

let _ = Common.Util.run_main main

(* TODO: remove this, its just a demo/for testing. In the end, the executable will live in each test repo. *)

open Ast_check

let main (_cmd : string) args =
  (* In the future, one might want to accept options,
     but [lp-ast-check -- file ...] will remain compatible. *)
  let args = match args with
  | "--" :: tl -> tl
  | _ -> args
  in
  let status = ref 0 in
  let k = function
    | [] -> ()
    | vs ->
        status := max 1 !status;
        let open Fmt in
        pr "@[<v>%a@]" (list ~sep:cut (box pp_violation)) vs
  in
  args |> List.iter (path_violations k);
  !status

let () =
  Sys.argv
  |> Array.to_list
  |> (function cmd :: args -> main cmd args | [] -> failwith "empty argv")
  |> exit

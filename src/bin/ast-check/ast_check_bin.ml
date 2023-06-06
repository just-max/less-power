open Common.Util

let main (args : string list) : int =
  let vss =
    args
    |> List.to_seq
    |> Seq.concat_map Common.Path_util.walk
    |> Seq.filter
      (fun p -> Filename.check_suffix p ".ml" && not (Sys.is_directory p))
    |> Seq.map Ast_check.file_violations
    |> List.of_seq
  in
  List.iter
    (Format.fprintf Format.std_formatter "%a" Ast_check.format_violations)
    vss ;
  List.fold_left
    Result.(fun a -> max a % function Ok [] -> 0 | Ok _ -> 1 | Error _ -> 2)
    0 vss

let () = Sys.argv |> Array.to_list |> List.tl |> main |> exit

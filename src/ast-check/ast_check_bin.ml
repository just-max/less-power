open Common.Util

let main (args : string list) : int =
  (* check all files in (the directories pointed to by) args *)
  let vss =
    args
    |> List.to_seq
    |> Seq.concat_map Common.Path_util.walk
    |> Seq.filter
      (fun p -> Filename.check_suffix p ".ml" && not (Sys.is_directory p))
    |> Seq.map Ast_check.file_violations
    |> Seq.memoize
  in
  (* the formatter for violations only works until a new file is parsed:
     see the comment for [is_quotable_loc] is location.ml, hence the first
     traversal of the sequence must do the printing *)
  Seq.iter
    (Format.fprintf Format.std_formatter "%a" Ast_check.format_violations)
    vss ;
  (* exit with 2 if an exception occurred while parsing, with 1 if there
     was a non-empty list of violations, and 0 otherwise *)
  Seq.fold_left
    Result.(fun a -> max a % function Ok [] -> 0 | Ok _ -> 1 | Error _ -> 2)
    0 vss

let () = Sys.argv |> Array.to_list |> List.tl |> main |> exit

open struct
  module Util = Common.Internal.Util
  module R = Result
  module S = Common.Internal.Syntax
  module C = Common.Error_context
end

open Util
open! Format

type c = [ Util.access_file_error | Ast_check.c ]

(** Formats the cause and ends with a flush+newline! Without flushing the
    pretty-printer, Location.report_exception messes with formatted output. *)
let format_cause fmt : c -> unit = function
  | `Access_file (_, e) -> fprintf fmt "%s@." e
  | `Iter_AST e -> fprintf fmt "Error iterating AST: %s@." (Printexc.to_string e)
  | `Parse_file (p, e) -> fprintf fmt "Error parsing file '%s':@ @[%a@]" p Location.report_exception e
  | `Unix_error (e, _, "") -> fprintf fmt "%s@." (Unix.error_message e)
  | `Unix_error (e, _, p) -> fprintf fmt "%s: %s@." p (Unix.error_message e)

let is_ml_file p =
  let open S.Result in
  if Filename.check_suffix p ".ml"
    then let+ is_dir = Util.is_directory p in not is_dir
    else C.One.ok false

(** given a path, if it points to a directory, return a sequence of all
    the OCaml source files it contains; otherwise return the singleton sequence
    with only the given path *)
let src_files_for_path p =
  let open S.Error_context.Many in
  let* is_dir = C.Many.of_one @@ Util.is_directory p in
  let+ src_file =
    if is_dir then
      Common.Internal.Path_util.walk p
      |> Seq.filter R.((fun p -> bind p is_ml_file) %> value ~default:true)
    else C.Many.return p
  in
  src_file, is_dir

let vs_for_src_file ?in_dir src =
  let vs = Ast_check.file_violations src in
  match in_dir with
  | Some d -> vs |> C.One.with_err_msg' (fun () -> Printf.sprintf "in directory '%s'" d)
  | None -> vs

(** check all files in (the directories pointed to by) args *)
let main (args : string list) : int =

  let vs : _ C.Many.t =
    Seq.memoize @@
    let open S.Error_context.Many in
    let* arg = C.Many.of_seq @@ List.to_seq args in
    let* src_file, is_in_dir = (src_files_for_path arg :> (_, c) C.Many.t) in
    let in_dir = if is_in_dir then Some arg else None in
    let vs = (vs_for_src_file ?in_dir src_file :> (_, c) C.One.t) in
    C.Many.of_one_seq (vs |> Result.map List.to_seq)
  in

  let format1 fmt = function
    | Ok v -> fprintf fmt "@[<hv>%a@]" Ast_check.format_violation v
    | Error (msgs, c) ->
        fprintf fmt "@[<hv>@[<hv 2>%a@]:@ @[%a@]@]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ":@ ") pp_print_string)
          ("lp-ast-check" :: msgs)
          format_cause c
  in

  (* the formatter for violations only works until a new file is parsed:
     see the comment for [is_quotable_loc] in location.ml; therefore the first
     traversal of the sequence must do the printing *)
  printf "%a" (pp_print_seq ~pp_sep:pp_print_newline format1) vs ;

  (* exit with 2 if an exception occurred while parsing, with 1 if there
     was a non-empty list of violations, and 0 otherwise *)
  Seq.fold_left
    Result.(fun a v -> max a @@ match v with Ok _ -> 1 | Error _ -> 2)
    0 vs

let () = Sys.argv |> Array.to_list |> List.tl |> main |> exit

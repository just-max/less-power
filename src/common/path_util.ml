(** Path and file-related utility functions. *)

open struct
  module C = Error_context
  module S = Syntax
end

open Unix
open Util

type walk_order = BottomUp | TopDown

let walk ?max_depth ?(walk_order = TopDown) ?(follow_symlinks = false)
  : string -> (string, Util.access_file_error) C.Many.t =

  (* whether we should descend recursively, or just yield p *)
  let should_read p =
    let open S.Result in
    let* sym_ok =
      if follow_symlinks then C.One.ok true
      else
        let+ stat = Util.lstat p in
        stat.st_kind <> Unix.S_LNK
    in
    let+ dir = Util.is_directory p in (* TODO: don't check is_directory if sym_ok is false *)
    sym_ok && dir
  in

  let rec impl depth p () =
    let open S.Error_context.Many in
    ~$() @@
    match max_depth with
    | Some md when depth >= md -> C.Many.return p
    | _ ->
        let* read_p = C.Many.of_one @@ should_read p in
        if read_p then
          let walk_entries () =
            ~$() @@
            let* dir_entry = C.Many.of_one_seq @@ Util.readdir p in
            let p' = Filename.concat p dir_entry in
            impl (depth + 1) p'
          in
          match walk_order with
          | BottomUp -> Seq.append walk_entries (C.Many.return p)
          | TopDown -> Seq.append (C.Many.return p) walk_entries
        else C.Many.return p
  in

  impl 0

(* TODO: make this fail or do something sensible if the second path is absolute...
   the standard library just concatenates *)
let ( / ) = Filename.concat

(** like {!Sys.readdir}, but the returned entries are full paths *)
let readdir_p p = Sys.readdir p |> Array.to_seq |> Seq.map (fun p1 -> p / p1)

(** Checks that either [p] is a regular file and [condition] holds for it,
    or [p] is a symlink to a regular file, and [condition] holds for the symlink. *)
let is_code ?(condition = FileUtil.True) p =
  let open FileUtil in
  if test Is_link p
  then test condition p && test Is_file (Unix.readlink p)
  else test (And (Is_file, condition)) p

(** Path and file-related utility functions. *)

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

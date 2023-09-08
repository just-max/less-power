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

let[@tail_mod_cons] rec parents path =
  match FilePath.dirname path with
  | "" -> []
  | parent -> parent :: parents parent

let default_mode = 0o777

let mkdir ?(mode = default_mode) ?parents:(pt = false) ?(exist_ok = false) p =
  let mkdir1 mode path =
    let open Unix in
    try mkdir path mode with Unix_error (EEXIST, _, _) when exist_ok -> ()
  in
  if pt then parents p |> List.rev |> List.iter (mkdir1 default_mode);
  mkdir1 mode p

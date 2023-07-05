(** Path and file-related utility functions. *)

open Util

type walk_order = BottomUp | TopDown

let walk ?max_depth ?(walk_order = TopDown)
  (* ?(follow_symlinks = false) TODO, currently always follows symlinks *) =
  let open Sys in
  let open Filename in
  let rec impl depth p : string Seq.t = fun () ->
    match max_depth with
    | Some md when depth >= md -> Seq.return p ()
    | _ ->
      if is_directory p then
        let walk_subdirs = 
          readdir p |> Array.to_seq
          |> Seq.concat_map (concat p %> impl (depth + 1))
        in
        match walk_order with
        | BottomUp -> Seq.Cons (p, walk_subdirs)
        | TopDown -> Seq.append walk_subdirs (Seq.return p) ()
      else
        Seq.return p ()
  in
  impl 0

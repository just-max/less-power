
(** Check if two lists contain the same items with the same multiplicities,
    but in any order. Consider items equal if [eq x y] (default: [x = y]) holds. *)
let rec equal_unordered ?(eq = (=)) xs ys =
  match xs with
  | [] -> ys = []
  | x :: xs' ->
    match List.partition (eq x) ys with
    | [], _ -> false
    | _ :: ys1', ys2' -> equal_unordered ~eq xs' (ys1' @ ys2')

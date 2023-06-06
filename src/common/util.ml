
(** Function composition, [f % g] is {m f \circ g}. *)
let (%) f g x = f (g x)

(** Reverse function composition, [f %> g] is {m g \circ f}. *)
let (%>) f g x = g (f x)

(** [peek f x] is [(f x; x)]. *)
let peek f x = f x; x

(** One-element list. *)
let singleton x = [x]

(** [string_contains ~needle haystack]:
    does [needle] exist as a substring of [haystack]?
    Naive {m \mathcal{O}(nm) } implementation. *)
let string_contains ~needle haystack =
  Seq.ints 0
  |> Seq.take String.(max 0 @@ length haystack - length needle + 1)
  |> Seq.map (fun i -> String.sub haystack i (String.length needle))
  |> Seq.exists ((=) needle)

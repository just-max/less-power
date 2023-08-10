(** More pretty-printing combinators. *)

let pp_of pp f : _ Fmt.t = fun ppf x -> pp ppf @@ f x

let pp_repeat ?(sep = Fmt.nop) n0 (pp : 'a Fmt.t) : 'a Fmt.t = fun ppf x ->
  let rec loop n =
    if n = 0 then ()
    else if n = 1 then pp ppf x
    else (pp ppf x; sep ppf (); loop (n - 1))
  in
  loop n0

let pp_text ?(line = Fmt.string) : string Fmt.t =
  let open Fmt in
  vbox @@ pp_of (list line) (String.split_on_char '\n')

let pp_flow ?(word = Fmt.string) : string Fmt.t =
  let open Fmt in
  pp_text ~line:(box @@ pp_of (list ~sep:sp word) (String.split_on_char ' '))

let pp_text_ = pp_text ?line:None
let pp_flow_ = pp_flow ?word:None

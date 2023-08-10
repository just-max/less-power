(** Miscellaneous utility functions. *)

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

(** [(~$ x) f] is [f x], e.g.:
    [List.map (~$ 2) [List.nth ["a"; "b"; "c"]; String.sub "def" 1]]
    returns [["c"; "ef"]] *)
let ( ~$ ) = ( |> )

let run_main main =
  Sys.argv
  |> Array.to_list
  |> (function cmd :: args -> main cmd args | [] -> failwith "empty argv")
  |> exit

open struct module C = Error_context end

type access_file_error = [
  | `Access_file of string * string (** path, [Sys_error] exception text *)
  | `Unix_error of Unix.error * string * string (** Arguments as to {!Unix.Unix_error} *)
]

(** Misc. helpers *)

let ctx_file_access f (p : string) : (_, access_file_error) C.One.t =
  try C.One.ok (f p) with
  | Sys_error e -> C.One.error (`Access_file (p, e))
  | Unix.Unix_error (e, f, a) -> C.One.error (`Unix_error (e, f, a))

let is_directory = ctx_file_access Sys.is_directory

let readdir : _ -> _ C.One.t = ctx_file_access Sys.readdir %> Result.map Array.to_seq

let lstat = ctx_file_access Unix.lstat

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

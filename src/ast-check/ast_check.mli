(** AST checker.

    Tool to enforce restrictions on what syntax elements are allowed in OCaml code.
    Restricts use of the following class features and imperative elements:

    - class declarations
    - class type declarations
    - class method calls
    - new class expressions
    - setting class and record fields
    - array literals
    - [while] loops
    - [for] loops
    - declaring records with mutable entries
    - [external] declarations
    - internal dune modules ([Package__Module])

    [external] declarations and internal dune modules are forbidden, as they can
    be used to circumvent restrictions in the [Stdlib] replacement.
    The sequence operator is not forbidden as there would be no point,
    [a; b] can be trivially replaced by [let _ = a in b].

    An executable is installed as [lp-ast-check]. All [.ml] files passed as
    arguments, or contained recursively in directories passed as arguments,
    are checked. Exits with code [0] if no violations are detected, [1] if
    violations are found, and [2] if an error occurs.
*)

(** A violation that occurred in the AST. *)
type violation = {
  location : Location.t ;
  (** Location of the violation. *)
  message : string option ;
  (** Error message. *)
}

type violations = (violation list, exn) Result.t
(** A list of violations that occurr in an AST, or an exception that occurred
    while parsing that AST. An empty list indicates no violations,
    and thus success. *)

(** Return a list of (up to [limit]) violations for this AST. *)
val ast_violations : ?limit:int -> Parsetree.structure -> violations

(** As per {!ast_violations}, but first parse a file to an AST. *)
val file_violations : ?limit:int -> string -> violations

(** Pretty-print the violations for a file. Should be used before the next
    file is parsed, otherwise no context from the source file will be output. *)
val format_violations : Format.formatter -> violations -> unit
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

    An executable is installed as [lp-ast-check]. All files passed as arguments,
    and all [.ml] files contained recursively in directories passed as arguments,
    are checked. Exits with code [0] if no violations are detected, [1] if
    violations are found, or [2] if an error occurs.
*)

open Ppxlib

module Feature :
  sig
    type t =
      Array | Mutable_member | Object | Loop
      | Primitive | Internal_name | Tail_mod_cons | Other
    module Set : Set.S with type elt = t

    val minimum : Set.t
    (** {!Primitive} and {!Internal_name}, required to
        prevent circumventing restrictions. *)

    val default : Set.t
    (** Everything but {!Tail_mod_cons}. *)

    val all : Set.t
    (** Everything. *)

    val to_message : t -> string
    (** Provide a message explaining why a feature is prohibited. *)
  end

(** A violation that occurred in the AST. *)
type violation = {
  location : Location.t ;
  (** Location of the violation. *)
  message : string option ;
  (** Error message. *)
  feature : Feature.t;
  (** Which prohibited feature was used. *)
}

(** Return a list of (up to [limit]) violations for this AST. *)
val ast_violations :
  ?prohibited:Feature.Set.t -> ?limit:int -> structure -> violation list

(** As per {!ast_violations}, but first parse a file to an AST. *)
val file_violations :
  ?prohibited:Feature.Set.t -> ?limit:int -> string -> violation list

(** As per {!file_violations}, but scan an entire directory (recursively).
    If the path designates a regular file, check the file if [check1] matches
    (default: always); if a directory is scanned, check each file if
    [check] matches (default: [.ml] ending). Pass each (possibly empty) list of
    violations to the callback; see the note in {!pp_violation}.

    The [follow] argument (default: {{!FilePath.Follow}[Follow]}) only applies to directories;
    symlinks to files are always read. *)
val path_violations :
  ?follow:FileUtil.action_link ->
  ?prohibited:Feature.Set.t -> ?limit:int ->
  ?check1:FileUtil.test_file -> ?check:FileUtil.test_file ->
  (violation list -> unit) -> string -> unit
  (* TODO: the callback could take the path, too... *)

(** Pretty-print the message of a violation, containing a generic message
    that describes the {{!violation.feature}feature} and the specific
    {{!violation.message}message} of the violation.*)
val pp_violation_message : Format.formatter -> violation -> unit

(** Pretty-print a violation. Violations from a given file should be printed
    before the next file is parsed, otherwise no context from the source file
    will be output.

    Breaks pretty-printing, this is a limitation of {!Location.print_report} *)
val pp_violation : Format.formatter -> violation -> unit

(** {2 Ppxlib transformations}
    Transformations intended directly for use with Ppxlib. *)

type 'a transformation = Expansion_context.Base.t -> 'a -> 'a

(** As per {!ast_violations}, but intended directly for use as a Ppxlib
    transformation. Errors are embedded as extension nodes in the returned AST. *)
val ast_violations_transformation :
  ?prohibited:Feature.Set.t -> ?limit:int -> Ppxlib.structure transformation

(** Removes module type annotations on modules.
    For example, these two definitions:
    {[
      module M : S = struct ... end
      module M : S with type t = ... = struct ... end
    ]}
    Both become:
    {[ module M = struct ... end ]}

    Allows testing code that annotated module types but didn't think
    to add the [with] constraint to expose the type. *)
val strip_signatures : Ppxlib.Ast.structure transformation

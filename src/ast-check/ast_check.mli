type violation = {
  location : Location.t ;
  (** Location of the violation. *)
  message : string option ;
  (** Error message. *)
}

type violations = (violation list, exn) Result.t

val ast_violations : ?limit:int -> Parsetree.structure -> violations
val file_violations : ?limit:int -> string -> violations
val format_violations : Format.formatter -> violations -> unit

(** Ast checker / sanitizer

    tool to enforce restrictions on what syntax elements are allowed in student code.
    by default restricts use of classes and imperative elements

    - Class declarations
    - Class type declarations
    - Class method calls
    - new class expressions
    - setting class and record fields
    - array literals
    - while loops
    - for loops
    - declaring records with mutable entries
    - external declarations

    external is forbidden as it can be used to circumvent restrictions in the stdlib replacement
    the sequence operator is not forbidden as there is no point,
    "a; b" can be trivially replaced by "let _ = a in b"
*)

open Parsetree

open Common.Util
module R = Result


module Messages = struct
  let default = "object oriented or imperative feature"
  let array = "array syntax"
  let mutable_record = "mutable record field"
  let class_features = "class-related feature"
  let loops = "loop"
  let external_def = "external definition"
end


(** A violation that occurred in the AST. *)
type violation = {
  location : Location.t ;
  (** Location of the violation. *)
  message : string option ;
  (** Error message. *)
}

let report_of_violation { location ; message } =
  let main = Location.msg ~loc:location "%s"
    (Option.value ~default:Messages.default message)
  in
  Location.{ kind = Report_error ; main ; sub = [] }

let violation ?message location = { location ; message }

type violations = (violation list, exn) Result.t


(** Context while iterating over an AST. *)
type context = {
  on_violation : violation -> unit
}

(** Helper for creating iterators that check for forbidden elements.
    Do not pass [iter] and [elem], they are part of the iterator type
    @param default The default iterator for this element,
      to call after checking for violations.
    @param violations Return a list of violations in this element.
*)
let iter_violations ctx default violations iter elem =
  List.iter ctx.on_violation (violations elem);
  default iter elem

(* Functions that return a list of violations for a given AST element,
   for use with [iter_violations] *)

let expr_violations expression =
  let open Messages in
  let[@warning "-16"] violation ?message =
    violation expression.pexp_loc ?message |> singleton
  in
  match[@warning "-4"] expression.pexp_desc with
  | Pexp_array _ -> violation ~message:array
  | Pexp_while _ | Pexp_for _ -> violation ~message:loops
  | Pexp_coerce _ | Pexp_send _ | Pexp_new _ ->
    violation ~message:class_features
  | Pexp_setinstvar _ -> violation ~message:mutable_record
  | Pexp_setfield _ -> violation ?message:None
  | _ -> []

let type_declaration_violations = function[@warning "-4"]
  | { ptype_kind = Ptype_record entries ; _ } ->
      List.filter_map
        (function
          | { pld_mutable = Asttypes.Mutable ; pld_loc = loc ; _ } ->
            Some (violation loc ~message:Messages.mutable_record)
          | _ -> None)
        entries
  | _ -> []

let structure_item_violations = function[@warning "-4"]
  | { pstr_desc = Pstr_primitive _ ; pstr_loc } ->
    violation ~message:Messages.external_def pstr_loc |> singleton
  | _ -> []

let always_violation ?message get_location x =
  violation ?message (get_location x) |> singleton


let violations_iterator ctx =
  Ast_iterator.{
    Ast_iterator.default_iterator with
    class_declaration = iter_violations ctx
      Ast_iterator.default_iterator.class_declaration
      (always_violation ~message:Messages.class_features (fun c -> c.pci_loc));
    class_type_declaration = iter_violations ctx
      Ast_iterator.default_iterator.class_type_declaration
      (always_violation ~message:Messages.class_features (fun c -> c.pci_loc));
    expr = iter_violations ctx
      Ast_iterator.default_iterator.expr expr_violations;
    type_declaration = iter_violations ctx
      Ast_iterator.default_iterator.type_declaration type_declaration_violations;
    structure_item = iter_violations ctx
      Ast_iterator.default_iterator.structure_item structure_item_violations;
  }


exception Violation_limit

(** Return a list of violations for this file. *)
let ast_violations ?limit ast =
  if limit = Some 0 then R.Ok [] else (* you do you... *)
  let open Ast_iterator in
  let violations = ref [] in
  let count = ref 0 in
  let iterator = violations_iterator
    { on_violation = fun v ->
        violations := v :: !violations ;
        limit |> Option.iter (fun l ->
          incr count ;
          if !count >= l then raise Violation_limit) ;
    }
  in
  match iterator.structure iterator ast with
    | () | exception Violation_limit -> R.Ok (List.rev !violations)
    | exception e -> R.Error e

let file_violations ?limit file_name =
  Pparse.parse_implementation ~tool_name:"lp-ast-check" file_name
  |> ast_violations ?limit


let format_violations fmt = function
  | Result.Ok vs ->
    List.iter (fun v -> Location.print_report fmt (report_of_violation v)) vs
  | Error exn -> Location.report_exception fmt exn

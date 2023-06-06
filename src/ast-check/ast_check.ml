open Parsetree
module R = Result

open Common.Util


module Messages = struct
  let default = "object oriented or imperative feature"
  let array = "array syntax"
  let mutable_record = "mutable record field"
  let class_features = "class-related feature"
  let loops = "loop"
  let external_def = "external definition"
  let internal_name = "internal module name"
end


type violation = {
  location : Location.t ;
  message : string option ;
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
let iter_violations ctx violations default iter elem =
  (* let vs = violations elem in *)
  (* print_endline @@ Printf.sprintf "%s (length vs)=%d" info (List.length vs); *)
  (* ctx.on_violation (violation ~message:info Location.none) ; *)
  List.iter ctx.on_violation (violations elem) ;
  default iter elem

(* Functions that return a list of violations for a given AST element,
   for use with [iter_violations] *)

let expr_violations expr =
  let open Messages in
  let[@warning "-16"] violation ?message =
    violation expr.pexp_loc ?message |> singleton
  in
  match[@warning "-4"] expr.pexp_desc with
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


(** The follwing get_idents_X functions, together, cover uses of identifiers
    ({!Longident.t}) in the AST. Pass these functions to [ident_violations]. *)

let get_idents_expr expr = match[@warning "-4"] expr.pexp_desc with
  | Pexp_ident id | Pexp_construct (id, _) | Pexp_field (_, id)
    | Pexp_setfield (_, id, _) | Pexp_new id -> [id]
  | Pexp_record (ids, _) -> List.map fst ids
  | _ -> []

let get_idents_typ t = match[@warning "-4"] t.ptyp_desc with
  | Ptyp_constr (id, _) | Ptyp_class (id, _) -> [id]
  | Ptyp_package (id0, ids) -> id0 :: List.map fst ids
  | _ -> []

let get_idents_pat p = match[@warning "-4"] p.ppat_desc with
  | Ppat_construct (id, _) | Ppat_type id | Ppat_open (id, _) -> [id]
  | Ppat_record (ids, _) -> List.map fst ids
  | _ -> []

let get_idents_type_extension te = [te.ptyext_path]

let get_idents_extension_constructor exc =
  match[@warning "-4"] exc.pext_kind with | Pext_rebind id -> [id] | _ -> []

let get_idents_class_type class_t =
  match[@warning "-4"] class_t.pcty_desc with
  | Pcty_constr (id, _) -> [id] | _ -> []

let get_idents_class_expr class_expr =
  match[@warning "-4"] class_expr.pcl_desc with
  | Pcl_constr (id, _) -> [id] | _ -> []

let get_idents_module_type module_t =
  match[@warning "-4"] module_t.pmty_desc with
  | Pmty_ident id | Pmty_alias id -> [id] | _ -> []

let get_idents_with_constraint = function
  | Pwith_type (id, _) | Pwith_modtype (id, _)
    | Pwith_modtypesubst (id, _) | Pwith_typesubst (id, _) -> [id]
  | Pwith_module (id1, id2) | Pwith_modsubst (id1, id2) -> [id1; id2]

let get_idents_module_expr module_expr =
  match[@warning "-4"] module_expr.pmod_desc with
  | Pmod_ident id -> [id] | _ -> []

(** Reject names that look like they could be module names when they contain the
    substring ["__"]. Only names that start with an ASCII lowercase character
    are not considered modules. This rejects some harmless names like
    [let übung__1 = ...], but those are depracated anyway. *)
let is_internal_name s =
  String.length s > 0 && (s.[0] < 'a' || 'z' < s.[0])
  && string_contains ~needle:"__" s

let rec ident_contains_internal_name =
  let open Longident in
  function
  | Lident s -> is_internal_name s
  | Ldot (id, s) -> is_internal_name s || ident_contains_internal_name id
  | Lapply (id1, id2) ->
    ident_contains_internal_name id1 || ident_contains_internal_name id2

(** Check for identifiers containing an internal name,
    and turn them into violations. *)
let ident_violations get_idents elem =
  get_idents elem
  |> List.filter_map (fun Location.{ txt = id ; loc } ->
    if ident_contains_internal_name id
      then Some (violation ~message:Messages.internal_name loc)
      else None)
(* todo share code *)
let name_violations get_names elem =
  get_names elem
  |> List.filter_map (fun Location.{ txt ; loc } ->
    if is_internal_name txt
      then Some (violation ~message:Messages.internal_name loc)
      else None)

let always_violation ?message get_location x =
  violation ?message (get_location x) |> singleton


let violations_iterator ctx =
  let open Location in
  let open Ast_iterator in

  let iter_vio_idents get_idents = iter_violations ctx (ident_violations get_idents) in
  let iter_vio_name get_name = iter_violations ctx (name_violations (get_name %> Option.to_list)) in
  let join_loc loc = loc.txt |> Option.map @@ fun txt -> { loc with txt } in

  (* The iterator needs to find:
     - All forbidden syntax constructs, obviously.
     - All identifiers ([Longident.t]), so that they can be checked
        against the rules for internal module names. This means
        finding all uses of [Longident.t] in {!module:Parsetree} and
        tracing them back to an AST element, to be found here.
    - All plain names ([string]), that could possibly share the name with a
        module (anything that's uppercase, e.g. variant constructors).
        This involves tracing uses of [string] in the AST.
        On their own, these would be harmless, by any use of them
        would trigger a violation by virtue of the previous rule. *)

  { default_iterator with

    class_declaration =
      default_iterator.class_declaration
      |> iter_violations ctx (always_violation
          ~message:Messages.class_features (fun c -> c.pci_loc))
      ;
    class_expr =
      default_iterator.class_expr
      |> iter_vio_idents get_idents_class_expr
      ;
    class_type =
      default_iterator.class_type
      |> iter_vio_idents get_idents_class_type
      ;
    class_type_declaration =
      default_iterator.class_type_declaration
      |> iter_violations ctx (always_violation
          ~message:Messages.class_features (fun c -> c.pci_loc))
      ;
    constructor_declaration =
      default_iterator.constructor_declaration
      |> iter_vio_name (fun cd -> Some cd.pcd_name)
      ;
    expr =
      default_iterator.expr
      |> iter_violations ctx expr_violations
      |> iter_vio_idents get_idents_expr
      |> iter_vio_name (function[@warning "-4"]
          | { pexp_desc = Pexp_letmodule (loc, _, _) ; _ } -> join_loc loc
          | _ -> None)
      ;
    extension_constructor =
      default_iterator.extension_constructor
      |> iter_vio_idents get_idents_extension_constructor
      |> iter_vio_name (fun ec -> Some ec.pext_name)
      ;
    module_binding =
      default_iterator.module_binding
      |> iter_vio_name (join_loc % fun mb -> mb.pmb_name)
      ;
    module_substitution =
      default_iterator.module_substitution
      |> iter_vio_idents (fun s -> [s.pms_manifest])
      |> iter_vio_name (fun ms -> Some ms.pms_name)
      ;
    module_expr =
      default_iterator.module_expr
      |> iter_vio_idents get_idents_module_expr
      |> iter_vio_name (function[@warning "-4"]
          | { pmod_desc = Pmod_functor (Named (loc, _), _) ; _ } -> join_loc loc
          | _ -> None)
      ;
    module_type =
      default_iterator.module_type
      |> iter_vio_idents get_idents_module_type
      |> iter_vio_name (function[@warning "-4"]
          | { pmty_desc = Pmty_functor (Named (loc, _), _) ; _ } -> join_loc loc
          | _ -> None)
      ;
    module_type_declaration =
      default_iterator.module_type_declaration
      |> iter_vio_name (fun mtd -> Some mtd.pmtd_name)
      ;
    module_declaration =
      default_iterator.module_declaration
      |> iter_vio_name (fun md -> join_loc md.pmd_name)
      ;
    open_description =
      default_iterator.open_description
      |> iter_vio_idents (fun d -> [d.popen_expr])
      ;
    pat =
      default_iterator.pat
      |> iter_vio_idents get_idents_pat
      |> iter_vio_name (function[@warning "-4"]
          | { ppat_desc = Ppat_unpack loc ; _ } -> join_loc loc | _ -> None)
      ;
    structure_item =
      default_iterator.structure_item
      |> iter_violations ctx structure_item_violations
      ;
    typ =
      default_iterator.typ |> iter_vio_idents get_idents_typ
      ;
    type_declaration =
      default_iterator.type_declaration
      |> iter_violations ctx type_declaration_violations
      ;
    type_extension =
      default_iterator.type_extension
      |> iter_vio_idents get_idents_type_extension
      ;
    value_description =
      default_iterator.value_description
      |> iter_vio_name (fun vd -> Some vd.pval_name)
      ;
    with_constraint =
      default_iterator.with_constraint
      |> iter_vio_idents get_idents_with_constraint
  }


exception Violation_limit

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
  | R.Ok vs ->
    List.iter (fun v -> Location.print_report fmt (report_of_violation v)) vs
  | R.Error exn -> Location.report_exception fmt exn

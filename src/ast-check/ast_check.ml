open Common.Util

module Messages = struct
  let array = "This is a use of array syntax, which is not permitted"
  let mutable_member = "This is a use of a mutable field or value, which is not permitted"
  let object_oriented = "This is a use of an object-oriented feature, which is not permitted"
  let loop = "This is a use of a loop, which is not permitted"
  let primitive = "This is a use of an external definition, which is not permitted"
  let internal_name =
    "This identifier contains a name that starts with an Uppercase letter \
     and contains Two__Underscores in a row\n\
     The use of identifiers of this form is not permitted"
  let tail_mod_cons =
    "This is a use of the 'Tail Modulo Constructor' \
     program transformation, which is not permitted"
  let other = "The use of this feature is not permitted"
end

module Feature = struct
  type t =
    Array | Mutable_member | Object | Loop
    | Primitive | Internal_name | Tail_mod_cons | Other

  (* TODO: add TMC to AST-checker tests (needs executable to support flags) *)

  module Set = Set.Make (struct type nonrec t = t let compare = compare end)

  let all =
    Set.of_list
      [ Array; Mutable_member; Object; Loop;
        Primitive; Internal_name; Tail_mod_cons; Other ]

  let minimum = Set.of_list [ Primitive; Internal_name ]
  let default = Set.remove Tail_mod_cons all

  let to_message : t -> string =
    let open Messages in
    function
    | Array -> array
    | Mutable_member -> mutable_member
    | Object -> object_oriented
    | Loop -> loop
    | Primitive -> primitive
    | Internal_name -> internal_name
    | Tail_mod_cons -> tail_mod_cons
    | Other -> other
end

type violation = {
  location : Ppxlib.Location.t ;
  message : string option ;
  feature : Feature.t ;
}

let violation location ?message feature = { location ; message; feature }
let violation1 location ?message feature = [violation location ?message feature]

(** Context while iterating over an AST. *)
type context = {
  on_violation : violation -> unit;
}

let iter_violations ctx violations =
  List.iter ctx.on_violation violations

module Patterns = struct
  open Ppxlib.Ast
  open Ppxlib.Ast_pattern
  open Feature

  let ( or ) = ( ||| )

  let exp_loc exp = exp.pexp_loc
  let str_loc str = str.pstr_loc

  let map1_violations loc feature =
    as__ %> map1 ~f:(fun x -> violation1 (loc x) feature)
  let no_violation () = drop |> map0 ~f:([] : violation list)

  let drop1 pat = pat drop
  let drop2 pat = pat drop drop
  let drop3 pat = pat drop drop drop
  (* let drop4 pat = pat drop drop drop drop *)
  let drop5 pat = pat drop drop drop drop drop

  let exp_violation () =
    let vio feat = map1_violations exp_loc feat in
    drop1 pexp_array |> vio Array
    or (drop2 pexp_while or drop5 pexp_for) |> vio Loop
    or
      (drop1 pexp_object or drop1 pexp_new or
        drop2 pexp_send or drop1 pexp_override)
      |> vio Object
    or (drop2 pexp_setinstvar or drop3 pexp_setfield) |> vio Mutable_member
    (* catch-all case, otherwise the pattern can fail *)
    or no_violation ()

  let str_violations () =
    let vio feat = map1_violations str_loc feat in
    drop1 pstr_primitive |> vio Primitive
    or (drop1 pstr_class or drop1 pstr_class_type) |> vio Object
    or no_violation ()

end

(** Reject names that look like they could be module names when they contain the
    substring ["__"]. Only names that start with an ASCII lowercase character
    are not considered modules. This rejects some harmless names like
    [let übung__1 = ...], but those are deprecated anyway. *)
let is_internal_name s =
  String.length s > 0 && (s.[0] < 'a' || 'z' < s.[0])
  && string_contains ~needle:"__" s

let rec ident_contains_internal_name =
  let open Ppxlib.Longident in
  function
  | Lident s -> is_internal_name s
  | Ldot (id, s) ->
      is_internal_name s || ident_contains_internal_name id
  | Lapply (id1, id2) ->
      ident_contains_internal_name id1 || ident_contains_internal_name id2

let iter_violations context =
  let open Ppxlib.Ast in
  let open Ppxlib.Location in

  let open Feature in
  let open Patterns in

  let iter ?loc sup f (ctx_loc : Ppxlib.Location.t) x =
    let loc' = Option.fold loc ~some:~$x ~none:ctx_loc in
    f loc' x; sup loc' x
  in
  let violation_when p feature location x =
    if p x then
      violation1 location feature
      |> iter_violations context
  in
  let violation_pat pat location x =
    Ppxlib.Ast_pattern.parse (pat ()) location x Fun.id
    |> iter_violations context
  in
  object
    inherit [Ppxlib.Location.t] Ppxlib.Ast_traverse.map_with_context as super

    (** Context: replace the current location with the given location. *)
    method! loc k _ located = super#loc k located.loc located

    (* For values that don't have their own location, update the location
       context as close to the values as possible (best-effort basis). *)

    (* mutable_flag may occur in these three places *)
    method! class_type_field _ ctf = super#class_type_field ctf.pctf_loc ctf
    method! class_field _ cf = super#class_field cf.pcf_loc cf
    method! label_declaration _ ld = super#label_declaration ld.pld_loc ld

    method! string =
      iter super#string @@ violation_when is_internal_name Internal_name

    method! longident =
      (* don't call super#longident here, to avoid warning multiple
         times for identifiers with multiple bad components *)
      iter (fun _ li -> li)
      @@ violation_when ident_contains_internal_name Internal_name

    method! expression =
      iter ~loc:exp_loc super#expression
      @@ violation_pat Patterns.exp_violation

    method! mutable_flag =
      iter super#mutable_flag
      @@ violation_when
          (function Mutable -> true | Immutable -> false)
          Mutable_member

    method! structure_item =
      iter ~loc:str_loc super#structure_item
      @@ violation_pat Patterns.str_violations

    method! attribute =
      iter ~loc:(fun attr -> attr.attr_loc) super#attribute
      @@ violation_when
          (fun attr ->
            List.mem attr.attr_name.txt
              ["tail_mod_cons"; "ocaml.tail_mod_cons"])
          Tail_mod_cons

  end

let ast_violations ?(prohibited = Feature.default) ?limit (ast : Ppxlib.structure) =
  if limit = Some 0 then [] else (* you do you... *)
  let violations = ref [] in
  let count = ref 0 in

  let exception Violation_limit in
  let iterator = iter_violations
    { on_violation = fun v ->
        if Feature.Set.mem v.feature prohibited then
          violations := v :: !violations ;
          limit |> Option.iter (fun l ->
            incr count ;
            if !count >= l then raise Violation_limit) ;
    }
  in
  match iterator#structure Ppxlib.Location.none ast with
  | _ | exception Violation_limit -> List.rev !violations


let file_violations ?prohibited ?limit path =
  Pparse.parse_implementation ~tool_name:"lp-ast-check" path
  |> ast_violations ?prohibited ?limit

let path_violations ?(follow = FileUtil.Follow) ?prohibited ?limit
    ?(check1 = FileUtil.True) ?(check = FileUtil.Has_extension "ml") k path =
  let open FileUtil in
  let cond = if test Is_file path then check1 else And (check, Is_file) in
  find ~follow cond path (fun () -> file_violations ?prohibited ?limit %> k) ()


let pp_violation ppf vio =
  let open Location in
  (* convert between Ppxlib and native types (why?) *)
  let loc = {
    loc_start = vio.location.Ppxlib.Location.loc_start;
    loc_end = vio.location.Ppxlib.Location.loc_end;
    loc_ghost = vio.location.Ppxlib.Location.loc_ghost;
  }
  in
  let report =
    let open Common.Pp_util in
    errorf ~loc "@[<v>%a%a@]"
      (pp_of pp_flow Feature.to_message) vio.feature
      Fmt.(option (cut ++ pp_flow)) vio.message
  in
  print_report ppf report

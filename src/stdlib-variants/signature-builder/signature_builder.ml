open Ppxlib
open Common.Util

(** Library and PPX rewriter for building interfaces from existing interfaces
    by selectively adding and removing interface items.

    See {!Parse} for the full grammar for inclusion specifications. *)

(** {1 Loading saved signatures} *)

type raw_signature = [
  | `IntfMarshal of string (** Marshalled {!Ppxlib.interface} *)
  | `IntfSrc of string (** Plain source code of an interface *)
]
type raw_signature_info = (string list * raw_signature) list

type signature_infos = (string * signature_info) list
and signature_info = Node of signature option * signature_infos

let rec infos_add_signature path0 path signature infos =
  update_assoc
    (Option.value ~default:(Node (None, []))
      %> info_add_signature path signature %> Option.some)
    path0 infos
and info_add_signature path signature (Node (node_signature, infos)) =
  match path with
  | [] -> Node (Some signature, infos)
  | p :: ps -> Node (node_signature, infos_add_signature p ps signature infos)

let rec infos_get_signature path0 path infos =
  Option.bind (List.assoc_opt path0 infos) (info_get_signature path)
and info_get_signature path (Node (s, infos)) =
  match path with
  | [] -> s
  | p :: ps -> infos_get_signature p ps infos

let load_raw_signature : raw_signature -> signature = function
  | `IntfMarshal m -> (Marshal.from_string m 0 : signature)
  | `IntfSrc s -> Ppxlib.Parse.interface (Lexing.from_string s)

let load_signature_info (raw_info : raw_signature_info) =
  List.fold_left
    (fun n (path, raw_sig) -> info_add_signature path (load_raw_signature raw_sig) n)
    (Node (None, [])) raw_info

(** {1 Signature of {!Stdlib} and {!Thread}/{!Event}} *)

let stdlib_signature_info = Stdlib_signature_info.signature_info |> load_signature_info

let threads_signature_info = Threads_signature_info.signature_info |> load_signature_info

(** {1 PPX rewriter} *)

[@@@warning "-4"]

module Ordered_set = struct
  type 'a t =
    | Univ
    | Empty
    | Singleton of 'a
    | Union of 'a t * 'a t
    | Minus of 'a t * 'a t

  let rec map f = function
    | Univ -> Univ
    | Empty -> Empty
    | Singleton x -> Singleton (f x)
    | Union (lhs, rhs) -> let lhs' = map f lhs in Union (lhs', map f rhs)
    | Minus (lhs, rhs) -> let lhs' = map f lhs in Minus (lhs', map f rhs)

  let eval ?(eq = (=)) univ s =
    let remove_from xs ys = List.filter (fun x -> not @@ List.exists (eq x) ys) xs in
    let rec eval = function
      | Univ -> univ
      | Empty -> []
      | Singleton x -> [x]
      | Union (lhs, rhs) ->
          let rhs_eval = eval rhs in
          remove_from (eval lhs) rhs_eval @ rhs_eval
      | Minus (lhs, rhs) ->
          remove_from (eval lhs) (eval rhs)
    in eval s
end

type kind =
  Value | Type | Exception | Module | ModuleType | Class | ClassType

type signature_item_pat = { names : longident_loc list; kind : kind option; loc : location }

type include_specs = include_spec list
and include_spec =
  | Include_from of { from : longident_loc; items : signature_item_pat Ordered_set.t; loc : Location.t }
  | Include_group of { attributes : attribute list; items : include_specs }

module Parse = struct

  (**

  Parser for include specifications.

  To use the signature builder, first enable the PPX rewriter.
  With dune, add the PPX rewriter to your component's [dune] file:
  {@dune[(preprocess (pps less-power.signature-builder-ppx))]}
  Then, within a signature, use an extension point to include signature items, for example:
  {[
  module type Foo : sig
    [%%include stdlib.stdlib (fst, snd)]
  end
  ]}

  Inclusion specifications can be more complex. The full grammar is as follows:

  {@text[
  include-specs := include-spec | '()' | '(' include-spec {',' include-spec} ')'
  include-spec :=
    | from-spec items-spec
    | '{' 'items =' include-specs ';' ['attributes = __' {attribute} ';'] '}'
  from-spec := (* qualified path to signature-info to take items from *)
    lowercase-ident {'.' lowercase-ident}+
  items-spec := (* ordered set of items *)
    item-spec | '!standard' | items-spec '-' items-spec | '()' | '(' items-spec {',' items-spec} ')'
  item-spec := item-names ['@' item-options]
  item-names := item-name | '[' {item-name ';'}+ ']'
  item-name := (* refer to values, types, modules, ... *)
    lowercase-ident | capitalized-ident | '[]'
  ]}

  The following more complex example includes [t] and [to_string_default]
  from the standard [Printexc] module, then also includes everything except
  that type and that value, with each item given the [unsafe] attribute:

  {[
  [%%include
    stdlib.printexc (t, to_string_default),
    { attributes = __ [@alert unsafe "This item is not permitted"];
      items = stdlib.printexc (!standard - (t, to_string_default)) }
  ]
  ]}

  To disambiguate signature items of different kinds with the same name,
  use the syntax [identifier @ { kind = "kind"}], where [kind] is one of
  [value], [type], [exception], [module], [module-type]. For example, to
  include the standard [ref] function:

  {[[%%include stdlib.stdlib (ref @ { kind = "value" })]]}

  The preprocessor will refuse to include types which don't include a type
  equation (e.g. abstract types and types defined only by constructors). This
  is because this will create copies of the type, which is generally not the intention.
  In this case, types must be defined manually without the inclusion mechanism, e.g.:

  {[type ('a, 'b) result = ('a, 'b) Stdlib.result = Ok of 'a | Error of 'b]}
  *)

  (* TODO: make it possible to reference an existing type automatically.
     Challenge is deciding how to resolve the type to reference. *)

  let ordered_set =
    let open Ordered_set in
    let rec impl exp = match exp with
      | [%expr !standard ] -> Univ
      | [%expr [%e? lhs] - [%e? rhs] ] -> Minus (impl lhs, impl rhs)
      | [%expr () ] -> Empty
      | { pexp_desc = Pexp_tuple exps; _ } ->
          let subsets = List.map impl exps in
          List.fold_right (fun lhs rhs -> Union (lhs, rhs)) subsets Empty
      | e -> Singleton e
    in
    impl

  let list exp0 =
    let[@tail_mod_cons] rec impl = function
      | [%expr [] ] -> []
      | [%expr ([%e? x] :: [%e? xs]) ] -> x :: impl xs
      | _ -> Location.raise_errorf ~loc:exp0.pexp_loc "Expected [a; list]"
    in impl exp0

  let kind exp =
    let options =
      [
        "value", Value;
        "type", Type;
        "exception", Exception;
        "module", Module;
        "module-type", ModuleType;
        "class", Class;
        "class-type", ClassType
      ]
    in
    match exp.pexp_desc with
    | Pexp_constant (Pconst_string (s, loc, _)) ->
        (match List.assoc_opt s options with
        | None ->
            Location.raise_errorf ~loc "Expected one of: %s"
              (List.map fst options |> String.concat ", ")
        | Some k -> k)
    | _ ->
        Location.raise_errorf ~loc:exp.pexp_loc "Expected one of: %s"
          (List.map (fst %> Printf.sprintf "%S") options |> String.concat ", ")

  let signature_item_pat exp0 : signature_item_pat =
    let p_ident exp =
      match exp.pexp_desc with
      | Pexp_ident name | Pexp_construct (name, None) -> name
      | _ -> Location.raise_errorf ~loc:exp.pexp_loc
          "Expected an_identifier or an Uppercase_identifier"
    in
    let p_idents exp = List.map p_ident (list exp) in
    let p_kind exp = match exp.pexp_desc with
      | Pexp_record (entries, None) ->
          let entries = List.map (fun (k, v) -> k.txt, v) entries in
          (match List.assoc_opt (Ppxlib.Lident "kind") entries with
            | None -> None
            | Some s -> Some (kind s))
      | _ -> Location.raise_errorf ~loc:exp.pexp_loc "Expected { kind = ... }"
    in
    (* empty list expression becomes a single name, not an empty list *)
    let p_names exp = match exp with
        | [%expr ([%e? _] :: [%e? _]) ] -> p_idents exp
        | _ -> [p_ident exp]
    in
    let names, kind = match exp0 with
      | [%expr [%e? names] @ [%e? args] ] -> p_names names, p_kind args
      | names -> p_names names, None
    in
    { names; kind; loc = exp0.pexp_loc }

  let dotted_name exp0 =
    let[@tail_mod_cons] rec impl exp = match exp.pexp_desc with
      | Pexp_field (e, { txt = Lident name; _}) -> Ldot (impl e, name)
      | Pexp_ident { txt = Lident name; _} -> Lident name
      | _ -> Location.raise_errorf ~loc:exp0.pexp_loc "Expected a.dotted.name"
    in
    impl exp0

  let rec include_specs = function
    | { pexp_desc = Pexp_tuple specs; _ } -> List.map (include_spec ~allow_comma:false) specs
    | [%expr () ] -> []
    | exp -> [include_spec ~allow_comma:true exp]

  and include_spec ~allow_comma = function
    | [%expr [%e? from_exp] [%e? items_exp] ] as exp ->
        let from = { txt = dotted_name from_exp; loc = from_exp.pexp_loc } in
        let items = ordered_set items_exp |> Ordered_set.map signature_item_pat in
        Include_from { from; items; loc = exp.pexp_loc }
    | { pexp_desc = Pexp_record (entries, None); _ } as exp ->
        (* TODO: error on extra and duplicate keys *)
        let entries = List.map (fun (k, v) -> k.txt, v) entries in
        let attributes = match List.assoc_opt (Ppxlib.Lident "attributes") entries with
          | None -> []
          | Some ([%expr __] as exp) -> exp.pexp_attributes
          | Some exp -> Location.raise_errorf ~loc:exp.pexp_loc "Expected '__ [@attrib ...] [@attrib ...]'"
        in
        let items = match List.assoc_opt (Ppxlib.Lident "items") entries with
          | None -> Location.raise_errorf ~loc:exp.pexp_loc "Missing key: items"
          | Some exp -> include_specs exp
        in
        Include_group { attributes ; items }
    | exp -> Location.raise_errorf ~loc:exp.pexp_loc
        "Expected 'module.name (item, item, ...)' or { attributes = ... ; items = ... }%s"
        (if allow_comma then " or (comma, separated, list) of the above" else "")
end

let is_signature_item_selectable sigi = match sigi.psig_desc with
  | Psig_open _ | Psig_include _ | Psig_attribute _ | Psig_extension _ -> false
  | _ -> true

type signature_item_name = Unnamed | Lident of longident | String of string

(** Does the given [longident] (as a search pattern) match the given name? *)
let match_lident_name (lident : longident) name = match lident, name with
  | Lident l, String n -> l = n
  | _, String _ -> false
  | l, Lident n -> l = n
  | _, Unnamed -> false

let signature_item_names sigi =
  let module_decl_name decl =
    match decl.pmd_name.txt with
    | Some name -> String name
    | None -> Unnamed
  in
  match sigi.psig_desc with
  | Psig_value desc -> [String desc.pval_name.txt]
  | Psig_type (_, decls) | Psig_typesubst decls -> List.map (fun desc -> String desc.ptype_name.txt) decls
  | Psig_typext ext -> [Lident ext.ptyext_path.txt]
  | Psig_exception exn -> [String exn.ptyexn_constructor.pext_name.txt]
  | Psig_module decl -> [module_decl_name decl]
  | Psig_modsubst subst -> [String subst.pms_name.txt]
  | Psig_recmodule decls -> List.map module_decl_name decls
  | Psig_modtype decl | Psig_modtypesubst decl -> [String decl.pmtd_name.txt]
  | Psig_class ds | Psig_class_type ds -> List.map (fun desc -> String desc.pci_name.txt) ds
  | Psig_open _ | Psig_include _ | Psig_attribute _ | Psig_extension _ -> []
  (* don't match open, include, @@@attribute, or %%extension: these are
      context-sensitive anyway, so should be included manually if needed *)

let match_kind k exp = match k, exp.psig_desc with
  | Value, Psig_value _ -> true
  | Type, (Psig_type _ | Psig_typesubst _ | Psig_typext _) -> true
  | Exception, Psig_exception _ -> true
  | Module, (Psig_module _ | Psig_modsubst _ | Psig_recmodule _) -> true
  | ModuleType, (Psig_modtype _ | Psig_modtypesubst _) -> true
  | Class, Psig_class _ -> true
  | ClassType, Psig_class_type _ -> true
  | _ -> false

let match_signature_item_pat (pat : signature_item_pat) sigi =
  (match pat.kind with Some k -> match_kind k sigi | None -> true)
  && list_equal (fun p s -> match_lident_name p.txt s) pat.names (signature_item_names sigi)

let eval_pat_ordered_set loc signature s =
  let indexed =
    signature |> List.filter is_signature_item_selectable
    |> List.mapi (fun i x -> i, (x, loc))
  in
  let lookup pat =
    match List.filter (snd %> fst %> match_signature_item_pat pat) indexed with
    | [i, (item, _)] -> i, (item, pat.loc)
    | [] -> Location.raise_errorf ~loc:pat.loc "Did not match any signature items"
    | items -> Location.raise_errorf ~loc:pat.loc
        "Ambiguous: matched %d signature items. Hint: use { kind = \"...\" } to disambiguate." (List.length items)
  in
  let check_psigi item loc =
    match item.psig_desc with
    | Psig_type (_, tdcls) ->
        tdcls |> List.iter (function
          | { ptype_manifest = None; ptype_name; _ } ->
              Location.raise_errorf ~loc
                ("Included type '%s' has no manifest. You should include it " ^^
                "manually and (optionally) specify a type equality, " ^^
                "e.g. type t = Foo.t")
                ptype_name.txt
          | _ -> ())
    | _ -> ()
  in
  Ordered_set.map lookup s
  |> Ordered_set.eval ~eq:(fun (i1, _) (i2, _) -> i1 = i2) indexed
  |> List.map snd
  |> List.map (fun (item, loc) -> check_psigi item loc; item)

let modify_attributes f sigi =
  let desc = match sigi.psig_desc with
    | Psig_value desc -> Psig_value { desc with pval_attributes = f desc.pval_attributes }
    | Psig_type (flag, decls) -> Psig_type (flag, List.map (fun decl -> { decl with ptype_attributes = f decl.ptype_attributes }) decls)
    | Psig_typesubst decls -> Psig_typesubst (List.map (fun decl -> { decl with ptype_attributes = f decl.ptype_attributes }) decls)
    | Psig_typext ext -> Psig_typext { ext with ptyext_attributes = f ext.ptyext_attributes }
      (* write to the ptyexn_constructor attributes instead of directly to
        ptyexn_attributes, because that's what @@alert wants; if this is a
        problem later then something smarter needs to be done *)
    | Psig_exception exn -> Psig_exception { exn with ptyexn_constructor = { exn.ptyexn_constructor with pext_attributes = f exn.ptyexn_constructor.pext_attributes } }
    | Psig_module decl -> Psig_module { decl with pmd_attributes = f decl.pmd_attributes }
    | Psig_modsubst subst -> Psig_modsubst { subst with pms_attributes = f subst.pms_attributes }
    | Psig_recmodule decls -> Psig_recmodule (List.map (fun decl -> { decl with pmd_attributes = f decl.pmd_attributes }) decls)
    | Psig_modtype decl -> Psig_modtype { decl with pmtd_attributes = f decl.pmtd_attributes }
    | Psig_modtypesubst decl -> Psig_modtypesubst { decl with pmtd_attributes = f decl.pmtd_attributes }
    | Psig_open desc -> Psig_open { desc with popen_attributes = f desc.popen_attributes }
    | Psig_include desc -> Psig_include { desc with pincl_attributes = f desc.pincl_attributes }
    | Psig_class descs -> Psig_class (List.map (fun desc -> { desc with pci_attributes = f desc.pci_attributes }) descs)
    | Psig_class_type decls -> Psig_class_type (List.map (fun decl -> { decl with pci_attributes = f decl.pci_attributes}) decls)
    | (Psig_attribute _ | Psig_extension _) as d -> d
  in
  { sigi with psig_desc = desc }

let rec eval_include_specs sig_infos specs =
  List.concat_map (eval_include_spec sig_infos) specs

and eval_include_spec sig_infos = function
  | Include_from { from; items; loc } ->
      let[@warning "-8"] p :: ps = Longident.flatten_exn from.txt in
      let signature =
        match infos_get_signature p ps sig_infos with
        | Some s -> s
        | None -> Location.raise_errorf ~loc:from.loc "Unknown signature"
      in
      eval_pat_ordered_set loc signature items
  | Include_group { attributes; items = spec_items } ->
      eval_include_specs sig_infos spec_items
      |> List.map (modify_attributes (fun attr -> attr @ attributes))

let default_sig_infos = [
  ("stdlib", stdlib_signature_info);
  ("threads", threads_signature_info);
]

(** PPX intended for use an a Ppxlib extender. The default [sig_infos] argument
    contains descriptions for the standard library under ["stdlib"]
    and for threads/events under ["threads"]. *)
let ppx_include ?(sig_infos = default_sig_infos) ~ctxt exp =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let module B = Ast_builder.Make (struct let loc = loc end) in
  try
    let sig_items = Parse.include_specs exp |> eval_include_specs sig_infos in
    B.psig_include (B.include_infos (B.pmty_signature sig_items))
  with e ->
    (* Note: the docs recommend embedding extension errors as deeply as
       possible. But that makes the code significantly more complex (everything
       has to be in Result.t), for no real gain (dev experience is fine as-is) *)
    match Location.Error.of_exn e with
    | Some loc_err -> B.psig_extension (Location.Error.to_extension loc_err) []
    | None -> raise e

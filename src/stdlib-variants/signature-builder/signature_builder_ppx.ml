open Ppxlib
open Ppxlib.Driver.V2

let _ =
  let include_extender =
    Extension.V3.declare "include"
      Extension.Context.signature_item
      Ast_pattern.(single_expr_payload __)
      (fun ~ctxt exp -> Signature_builder.ppx_include ~ctxt exp)
  in
  let include_rule = Context_free.Rule.extension include_extender in
  register_transformation ~rules:[include_rule] "build_signature"

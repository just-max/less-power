(** Use the AST check as a PPX rewriter, with arguments.
    If further customization is needed, it's best to define a PPX
    directly in the given exercise's repository instead. *)

open Ppxlib.Driver
open Ppxlib.Driver.V2
open Ast_check

let feature_identifiers =
  Feature.all |> Feature.Set.to_list |> List.map Feature.to_identifier

let prohibited_features = ref Feature.default

let update_feature ~prohibit ident =
  let open Feature in
  prohibited_features :=
    (if prohibit then Set.add else Set.remove)
      (of_identifier ident) !prohibited_features

let feature_spec ~prohibit =
  Arg.Symbol (feature_identifiers, update_feature ~prohibit)

let violation_limit = ref None

let strip_signatures = ref false

let () =
  add_arg "-prohibit-feature"
    (feature_spec ~prohibit:true)
    ~doc:" Prohibit the given feature";
  add_arg "-allow-feature"
    (feature_spec ~prohibit:false)
    ~doc:" Allow the given feature";
  add_arg "-violation-limit"
    (Arg.Int (fun l -> violation_limit := Some l))
    ~doc:" Maximum number of violations to report";
  add_arg "-no-violation-limit"
    (Arg.Unit (fun () -> violation_limit := None))
    ~doc:" Do not limit the number of reported violations (default)";

  (* While the generated PPX has an argument to disable transformations,
     there is no way to disable a transformation by default.
     Therefore, we implement our own disabled-by-default behaviour. *)
  add_arg "-strip-signatures"
    (Arg.Set strip_signatures)
    ~doc:"Enable the signature stripping transformation";
  add_arg "-no-strip-signatures"
    (Arg.Clear strip_signatures)
    ~doc:"Disable the signature stripping transformation (default)";

  register_transformation "ast_check"
    ~impl:(fun ctx ->
      Ast_check.ast_violations_transformation
        ~prohibited:!prohibited_features ?limit:!violation_limit ctx);

  register_transformation "strip_signatures"
    ~impl:(fun ctx ast ->
      if !strip_signatures then Ast_check.strip_signatures ctx ast else ast)

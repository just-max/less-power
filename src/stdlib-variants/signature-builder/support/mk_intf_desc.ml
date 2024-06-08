open Ppxlib

module Ast_builder_Make (Loc : Ast_builder.Loc) = struct
  include Ast_builder.Make (Loc)

  let pexp_string ?delim str =
    pexp_constant (Pconst_string (str, Loc.loc, delim))

  let pexp_list xs =
    List.fold_right
      (fun exp tail -> [%expr ( [%e exp] :: [%e tail] )]) xs [%expr []]
end

let loc = Location.none (* default location for metaquot *)
module B = Ast_builder_Make (struct let loc = Location.none end)

let parse_interface path =
  let open Common.Ctx_util in
  let< ch = In_channel.with_open_text path in
  let lexbuf = Lexing.from_channel ch in
  Lexing.set_filename lexbuf path;
  Ppxlib.Parse.interface lexbuf

let marshal_signature_to_string (s : signature) = Marshal.to_string s []

let[@warning "-32"] sig_info_for_path_marshal path =
  let name = Filename.(basename path |> remove_extension) in
  let intf_data = path |> parse_interface |> marshal_signature_to_string in
  [%expr ( [ [%e B.pexp_string name] ] , `IntfMarshal [%e B.pexp_string intf_data] ) ]

let[@warning "-32"] sig_info_for_path_src path =
  let name = Filename.(basename path |> remove_extension) in
  let intf_data =
    (Printf.sprintf "# 1 %S\n" path) ^
    In_channel.(with_open_text path input_all)
  in
  [%expr ( [ [%e B.pexp_string name] ] , `IntfSrc [%e B.pexp_string intf_data] ) ]

let sig_info_for_paths paths =
  List.map sig_info_for_path_marshal paths |> B.pexp_list

let main args =
  let sigs = sig_info_for_paths args in
  let info = [%stri let signature_info = [%e sigs] ] in
  Format.printf "%a\n" Ppxlib_ast.Pprintast.structure [ info ]

let _ = Common.Util.run_main (fun _ args -> main args; 0)

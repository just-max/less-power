type 'a clamp = { min : 'a; max : 'a }

let clamp range v = min range.max (max range.min v)
let clamp_opt range v = match range with None -> v | Some range -> clamp range v

type grading_result = { text : string; points : int; max_points : int }
type tests = (string * bool) list

type grading_criterion =
  | Passed of string
  | Failed of string
  | Not of grading_criterion
  | OneOf of grading_criterion list
  | AllOf of grading_criterion list
  | Constant of bool

let mk_indent n = String.make n ' '

let rec string_of_grading_criterion ?(indent = 0) = function
  | Passed test -> mk_indent indent ^ "Passed " ^ test
  | Failed test -> mk_indent indent ^ "Failed " ^ test
  | Not crit -> mk_indent indent ^ "Not:\n" ^ string_of_grading_criterion ~indent:(succ indent) crit
  | OneOf crits -> mk_indent indent ^ "OneOf:\n" ^ String.concat "\n" (List.map (string_of_grading_criterion ~indent:(succ indent)) crits)
  | AllOf crits -> mk_indent indent ^ "AllOf:\n" ^ String.concat "\n" (List.map (string_of_grading_criterion ~indent:(succ indent)) crits)
  | Constant const -> mk_indent indent ^ "Constant: " ^ string_of_bool const

let rec eval_criterion (tests : tests) = function
  | Passed test -> List.assoc test tests
  | Failed test -> List.assoc test tests |> not
  | Not crit -> eval_criterion tests crit |> not
  | OneOf crits -> List.exists (eval_criterion tests) crits
  | AllOf crits -> List.for_all (eval_criterion tests) crits
  | Constant const -> const

type grading =
  | Points of {
      title : string;
      points : int;
      test_case : grading_criterion;
      reason : tests -> grading_criterion -> string;
    }
  | Group of { title : string; items : grading list; max_points : int clamp option; skip : string option }
  | Conditional of { condition : grading_criterion; message : string; content : grading }

exception No_reason

let points ?(reason = fun _ _ -> None) ?penalty title points test_case =
  match (penalty, points < 0) with
  | Some false, _ | None, false ->
      Points
        {
          title;
          points;
          test_case;
          reason =
            (fun t c -> match reason t c with Some s -> s | None -> if eval_criterion t c then "PASS" else "FAIL");
        }
  | Some true, _ | None, true ->
      Points
        {
          title;
          points;
          test_case;
          reason =
            (fun t c ->
              match reason t c with Some s -> s | None -> if eval_criterion t c then "PENALTY" else raise No_reason);
        }

let assertion ?(message = "ASSERTION FAILED") ?(title = "assertion") points test_case =
  Points
    {
      title;
      points;
      test_case = Not test_case; (* fail the assertion if the test case DOES NOT pass *)
      reason = (fun t _ ->
        if eval_criterion t test_case then "PASS"
        else "\n\n" ^ message ^ ":\n" ^ string_of_grading_criterion test_case ^ "\n")
    }

let points_p ?reason ?penalty title point test_case = points ?reason ?penalty title point (Passed test_case)
let points_f ?reason ?penalty title point test_case = points ?reason ?penalty title point (Failed test_case)
let points_c ?reason ?penalty title point test_case = points ?reason ?penalty title point (Constant test_case)
let conditional condition message content = Conditional { condition; message; content }
let conditional_c condition message content = conditional (Constant condition) message content
let group ?skip ?max_points title items = Group { title; max_points; items; skip }

let implies antecedent consequent = OneOf [Not antecedent; consequent]

type tree = El of Xmlm.tag * tree list | Data of string

let in_tree i =
  let el tag childs = El (tag, childs) in
  let data d = Data d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let frag = function El (tag, childs) -> `El (tag, childs) | Data d -> `Data d in
  Xmlm.output_doc_tree frag o t

let map_tree f tree =
  let rec map tree = match f tree with Data d -> Data d | El (tag, childs) -> El (tag, List.map map childs) in
  map tree

let map_tree_data f tree =
  let rec map tree = match tree with Data d -> Data (f d) | El (tag, childs) -> El (tag, List.map map childs) in
  map tree

let fold_tree_down f acc tree =
  let rec map acc tree =
    match f acc tree with
    | acc, Data d -> (acc, Data d)
    | acc, El (tag, childs) ->
        let acc, childs =
          List.fold_left
            (fun (acc, childs) b ->
              let acc, c = map acc b in
              (acc, c :: childs))
            (acc, []) childs
        in
        (acc, El (tag, List.rev childs))
  in
  map acc tree

let map_xml file f =
  let inc = open_in_gen [ Open_binary; Open_creat; Open_rdonly ] 0 file in
  let inx = Xmlm.make_input (`Channel inc) in
  let dtd, tree = in_tree inx in
  let _ = close_in inc in
  let tree = map_tree f tree in
  let outc = open_out_bin file in
  let outx = Xmlm.make_output (`Channel outc) in
  let _ = out_tree outx (dtd, tree) in
  close_out outc

let keep_attribute_keys (keep : string list) : Xmlm.attribute list -> Xmlm.attribute list =
  List.filter_map (function (("", attr), _) when List.mem attr keep -> Some (("", attr), "") | _ -> None)

let std_attrs_testcase : Xmlm.attribute list =
  [ ("", "classname"), "" ; ("", "time"), "0.0" ]
let std_attrs_failure : Xmlm.attribute list =
  [ ("", "type"), "" ]

let empty_node ?(attributes = []) label =
  (* empty data, otherwise xmlm doesn't (self-)close empty tags! *)
  El ((("", label), attributes), [ Data "" ])

(** reads a whole file from disk as a string *)
  let prettify_results_basic fn =
  let read_file_whole fn =
    let ch = open_in_bin fn in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let data = read_file_whole fn in
  let data = Str.global_substitute (Str.regexp "<failure[^>]+?>\n?") (Fun.const {|<failure type="assert">|}) data in
  let data = Str.global_substitute (Str.regexp "\n*No backtrace.</failure>") (Fun.const "</failure>") data in
  let oc = open_out fn in
  output_string oc data;
  close_out oc

let prettify_results ?(grading : grading option) ?(points_step_count = 1) fn =
  let trim_message d =
    let d = Str.global_substitute (Str.regexp "\n+\\(No backtrace.\\)?$") (Fun.const "\n") d in
    String.trim d
  in
  let indent s =
    s |> String.split_on_char '\n' |> List.map (fun s -> if s = "" then "" else " " ^ s) |> String.concat "\n"
  in
  let rec map2' f l1 l2 fb1 fb2 =
    match (l1, l2) with
    | [], [] -> []
    | [], a2 :: l2 -> f fb1 a2 :: map2' f l1 l2 fb1 fb2
    | a1 :: l1, [] -> f a1 fb2 :: map2' f l1 l2 fb1 fb2
    | a1 :: l1, a2 :: l2 -> f a1 a2 :: map2' f l1 l2 fb1 fb2
  in
  let rec drop_last = function [] | [ _ ] -> [] | x :: xs -> x :: drop_last xs in
  let align_tabs text =
    let lines = text |> String.split_on_char '\n' |> List.map (String.split_on_char '\t') in
    let indents =
      lines |> List.map (List.map String.length) |> List.fold_left (fun acc v -> map2' max acc (drop_last v) 0 0) []
    in
    lines
    |> List.map (fun l ->
           let len = List.length l in
           List.mapi
             (fun i str -> if i = len - 1 then str else str ^ String.make (List.nth indents i - String.length str) ' ')
             l)
    |> List.map (String.concat "")
    |> String.concat "\n"
  in
  let pointf points = float_of_int points /. float_of_int points_step_count in
  let mk_points count passed =
    List.init count (fun n ->
        El
          ( (("", "testcase"), [ (("", "name"), "points:" ^ string_of_int n) ] @ std_attrs_testcase),
            if n < passed then [] else [ empty_node ~attributes:std_attrs_failure "failure" ] ))
  in
  let pprec =
    match points_step_count with
    | 1 -> format_of_string "%.0f"
    | 2 -> format_of_string "%.1f"
    | _ -> format_of_string "%.2f"
  in
  let rec collect_tests tests = function
    | Points { title; test_case; points; reason } -> (
        let max_points = max points 0 in
        match (eval_criterion tests test_case, reason tests test_case) with
        | true, s -> { text = Printf.sprintf "%s: \t%(%f%)P \t%s\n" title pprec (pointf points) s; points; max_points }
        | false, s ->
            { text = Printf.sprintf "%s: \t(%(%f%)P) \t%s\n" title pprec (pointf points) s; points = 0; max_points }
        | exception _ -> { text = ""; points = 0; max_points })
    | Group { title; items; max_points; skip } ->
        let results = List.map (collect_tests tests) items in
        let points = List.fold_left (fun a b -> a + b.points) 0 results |> clamp_opt max_points in
        let points_str = match skip with None -> Printf.sprintf "%(%f%)P" pprec (pointf points) | Some s -> s in
        {
          text =
            ((match (title, max_points) with
             | "root", _ -> Printf.sprintf "Total: %s\n" points_str
             | _, None -> Printf.sprintf "%s: \t%s\n" title points_str
             | _, Some { min = 0; max = max_points } ->
                 Printf.sprintf "%s: \t%s \t(max %(%f%)P)\n" title points_str pprec (pointf max_points)
             | _, Some max_points ->
                 Printf.sprintf "%s: \t%s \t(%(%f%)P - %(%f%)P)\n" title points_str pprec (pointf max_points.min) pprec
                   (pointf max_points.max))
            ^ if skip = None then results |> List.map (fun x -> indent x.text) |> String.concat "" else "");
          points;
          max_points = List.fold_left (fun a b -> a + b.max_points) 0 results |> clamp_opt max_points;
        }
    | Conditional { condition; content; _ } when eval_criterion tests condition -> collect_tests tests content
    | Conditional { content; message; _ } ->
        let result = collect_tests tests content in
        let name = String.split_on_char '\n' result.text |> List.hd |> String.split_on_char '\t' |> List.hd in
        { text = name ^ "\t" ^ message ^ "\n"; points = 0; max_points = result.max_points }
  in
  let analyze_tests tests =
    match grading with
    | None -> []
    | Some grading ->
        let { text; points; max_points } = collect_tests tests grading in
        let text = align_tabs text in
        Printf.printf
          "==============================================================================\n\
           %s\n\
           ------------------------------------------------------------------------------\n"
          text;
        El ((("", "testcase"), [ (("", "name"), "feedback") ] @ std_attrs_testcase),
          [ El ((("", "failure"), std_attrs_failure), [ Data text ]) ])
        :: mk_points max_points points
  in
  map_xml fn (function[@warning "-4"]
    | El (((("", ("failure" | "error")) as name), attrs), content) ->
        El ((name, keep_attribute_keys [ "type" ] attrs), content)
    | El (((("", "testcase") as name), attrs), content) ->
        let content = List.map (map_tree (function Data d -> Data (trim_message d) | v -> v)) content in
        El ((name, attrs), content)
    | El (((("", "testsuite") as name), attrs), content) as el ->
        let tests, _ =
          fold_tree_down
            (fun a b ->
              match b with
              | El ((("", "testcase"), attrs), children) -> (
                  match
                    ( List.find_map (function ("", "name"), v -> Some v | _ -> None) attrs,
                      not (List.exists (function El (_, _) -> true | _ -> false) children) )
                  with
                  | Some name, passed -> ((name, passed) :: a, b)
                  | _ -> (a, b))
              | _ -> (a, b))
            [] el
        in
        El ((name, attrs),
          List.filter (function El (((_, ("system-out" | "system-err")), _), _) -> false | _ -> true) content
          @ analyze_tests tests
          @ [ empty_node "system-out" ; empty_node "system-err" ])
    | v -> v)

(* Part 1: Grading *)

type 'a clamp = { min : 'a; max : 'a }

let clamp range v = min range.max (max range.min v)
let clamp_opt range v = match range with None -> v | Some range -> clamp range v

type grading_result = { text : string; points : int; max_points : int }
type tests = (string * bool) list (* testsuite *)
type testsuites = tests list

type grading_criterion =
  | Passed of string
  | Failed of string
  | Not of grading_criterion
  | OneOf of grading_criterion list
  | AllOf of grading_criterion list
  | Constant of bool

let matches_test ~query test =
  let rec impl qs ts = match qs, ts with
    | [], [] -> true
    | [""], _ -> true  (* query ended with trailing : *)
    | [], _ | _, [] -> false
    | q :: qs, t :: ts -> if q = "*" || q = t then impl qs ts else false
  in
  if query = [""] then test = [""]
  else impl query test

(* currently unused *)
let[@warning "-32"] is_fixed_query query =
  (query = [""] || List.(hd (rev query) <> "")) && List.for_all ((<>) "*") query

let parse_name = String.split_on_char ':'

let rec evaluate_criterion (tests : tests) = function
  | Passed query ->
      let q = parse_name query in
      let matched = List.filter (fun (test, _) -> matches_test ~query:q (parse_name test)) tests in
      if matched = [] then raise Not_found; (* at least one test needs to match *)
      List.for_all snd matched
  | Failed query ->
      evaluate_criterion (List.map (fun (test, ok) -> (test, not ok)) tests) (Passed query)
  | Not crit -> evaluate_criterion tests crit |> not
  | OneOf crits -> List.exists (evaluate_criterion tests) crits
  | AllOf crits -> List.for_all (evaluate_criterion tests) crits
  | Constant const -> const

let mk_indent n = String.make n ' '

let rec string_of_grading_criterion ?(indent = 0) = function
  | Passed test -> mk_indent indent ^ "Passed " ^ test
  | Failed test -> mk_indent indent ^ "Failed " ^ test
  | Not crit -> mk_indent indent ^ "Not:\n" ^ string_of_grading_criterion ~indent:(succ indent) crit
  | OneOf crits -> mk_indent indent ^ "OneOf:\n" ^ String.concat "\n" (List.map (string_of_grading_criterion ~indent:(succ indent)) crits)
  | AllOf crits -> mk_indent indent ^ "AllOf:\n" ^ String.concat "\n" (List.map (string_of_grading_criterion ~indent:(succ indent)) crits)
  | Constant const -> mk_indent indent ^ "Constant: " ^ string_of_bool const

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

let points ?(skip = Constant false) ?(reason = fun _ _ -> None) ?penalty title points test_case =
  let reason t c =
    if evaluate_criterion t skip then raise No_reason else
    match (penalty, points < 0) with
    | Some false, _ | None, false -> (
          match reason t c with
          | Some s -> s
          | None -> if evaluate_criterion t c then "PASS" else "FAIL")
    | Some true, _ | None, true -> (
          match reason t c with
          | Some s -> s
          | None -> if evaluate_criterion t c then "PENALTY" else raise No_reason)
  in
  Points { title; points; test_case; reason }

let assertion ?(message = "ASSERTION FAILED") ?(title = "assertion") points test_case =
  Points
    {
      title;
      points;
      test_case = Not test_case; (* fail the assertion if the test case DOES NOT pass *)
      reason = (fun t _ ->
        if evaluate_criterion t test_case then "PASS"
        else "\n\n" ^ message ^ ":\n" ^ string_of_grading_criterion test_case ^ "\n")
    }

let points_p ?skip ?reason ?penalty title point test_case = points ?skip ?reason ?penalty title point (Passed test_case)
let points_f ?skip ?reason ?penalty title point test_case = points ?skip ?reason ?penalty title point (Failed test_case)
let points_c ?skip ?reason ?penalty title point test_case = points ?skip ?reason ?penalty title point (Constant test_case)
let conditional condition message content = Conditional { condition; message; content }
let conditional_c condition message content = conditional (Constant condition) message content
let group ?skip ?max_points title items = Group { title; max_points; items; skip }

let implies antecedent consequent = OneOf [Not antecedent; consequent]

let any_passed test_case = Not (Failed test_case)
let any_failed test_case = Not (Passed test_case)

let evaluate_grading ?(points_step_count = 1) grading tests =
  let indent s =
    s |> String.split_on_char '\n'
    |> List.map (function "" -> "" | s -> " " ^ s)
    |> String.concat "\n"
  in
  let pointf points = float_of_int points /. float_of_int points_step_count in
  let pprec =
    match points_step_count with
    | 1 -> format_of_string "%.0f"
    | 2 -> format_of_string "%.1f"
    | _ -> format_of_string "%.2f"
  in
  let rec collect (* tests *) = function
  | Points { title; test_case; points; reason } -> (
      let max_points = max points 0 in
      match (evaluate_criterion tests test_case, reason tests test_case) with
      | true, s -> { text = Printf.sprintf "%s: \t%(%f%)P \t%s\n" title pprec (pointf points) s; points; max_points }
      | false, s ->
          { text = Printf.sprintf "%s: \t(%(%f%)P) \t%s\n" title pprec (pointf points) s; points = 0; max_points }
      | exception _ -> { text = ""; points = 0; max_points })
  | Group { title; items; max_points; skip } ->
      let results = List.map collect items in
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
  | Conditional { condition; content; _ } when evaluate_criterion tests condition -> collect content
  | Conditional { content; message; _ } ->
      let result = collect content in
      let name = String.split_on_char '\n' result.text |> List.hd |> String.split_on_char '\t' |> List.hd in
      { text = name ^ "\t" ^ message ^ "\n"; points = 0; max_points = result.max_points }
  in
  collect grading


(* Part 2: XML *)

type tree = El of Xmlm.tag * tree list | Data of string

let in_tree i =
  let el tag childs = El (tag, childs) in
  let data d = Data d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let frag = function El (tag, childs) -> `El (tag, childs) | Data d -> `Data d in
  Xmlm.output_doc_tree frag o t

type enter = NoEnter | Enter
let fold_down f =
  let rec map acc tree =
    match f acc tree with
    | acc, (Data _ as tree), _ | acc, tree, NoEnter -> (acc, tree)
    | acc, El (tag, childs), Enter ->
        let acc, childs = map_forest acc childs in
        (acc, El (tag, childs))
  and map_forest acc childs =
    let acc, childs =
      List.fold_left
        (fun (acc, childs) b ->
          let acc, c = map acc b in
          (acc, c :: childs))
        (acc, []) childs
    in
    (acc, List.rev childs)
  in
  map, map_forest

let fold_tree_down f = fst (fold_down f)
let fold_forest_down f = snd (fold_down f)

let read_tree path =
  let open Common.Ctx_util in
  let< ch = In_channel.with_open_gen [ Open_binary; Open_creat; Open_rdonly ] 0 path in
  let xml_in = Xmlm.make_input (`Channel ch) in
  in_tree xml_in

let write_tree dtd tree path =
  let open Common.Ctx_util in
  let< ch = Out_channel.with_open_bin path in
  let xml_out = Xmlm.make_output (`Channel ch) in
  out_tree xml_out (dtd, tree)

let keep_attribute_keys (keep : string list) : Xmlm.attribute list -> Xmlm.attribute list =
  List.filter_map (function (("", attr), _) when List.mem attr keep -> Some (("", attr), "") | _ -> None)

let trim_message d =
  let d = Str.global_substitute (Str.regexp "\n+\\(No backtrace.\\)?$") (Fun.const "\n") d in
  String.trim d


let extract_cleanup_tree : tree -> testsuites * tree =
  (* failure/error nodes: strip some unnecessary noise *)
  let message_node name attrs children =
    let f has_data = function
      | Data d -> true, Data (trim_message d), Enter
      | El _ as t -> has_data, t, Enter
    in
    let has_data, tree_content = fold_forest_down f false children in
    let new_content =
      if has_data then tree_content
      else
        match List.assoc_opt ("", "message") attrs with
          | None | Some "" -> []
          | Some msg -> [Data msg]
    in
    El ((name, keep_attribute_keys [ "type" ] attrs), new_content)
  in

  (* testcase nodes: return [Some (name, passed)] (if a name is found) *)
  let testcase attrs children =
    let f passed = function[@warning "-4"]
      | El (((("", ("failure" | "error")) as name), attrs), children) ->
          false, message_node name attrs children, NoEnter
      | t -> passed, t, Enter
    in
    let passed, children' = fold_forest_down f true children in
    let test =
      List.assoc_opt ("", "name") attrs
      |> Option.map (fun name -> name, passed)
    in
    test, children'
  in

  (* testsuite node: extract (reversed) list of tests, cleanup *)
  let testsuite node =
    let f tests = function[@warning "-4"]
      | El ((("", "testcase"), attrs as tag), children) ->
          let test, children' = testcase attrs children in
          (Option.to_list test @ tests), El (tag, children'), NoEnter
      | El ((("", ("system-out" | "system-err")), _ as tag), _) ->
          tests, El (tag, []), NoEnter
      | t -> tests, t, Enter
    in
    fold_tree_down f [] node
  in

  (* extract all test suites *)
  let testsuites root =
    let f suites = function[@warning "-4"]
      | El ((("", "testsuite"), _), _) as node ->
          let suite, node' = testsuite node in
          suite :: suites, node', NoEnter
      | t -> suites, t, Enter
    in
    fold_tree_down f [] root
  in

  testsuites

let extract_cleanup_file ?cleanup_to path =
  let dtd, tree = read_tree path in
  let suites, tree' = extract_cleanup_tree tree in
  (Option.iter (write_tree dtd tree') cleanup_to);
  suites

let extract_cleanup_files ?(cleanup = true) : string list -> testsuites =
  List.concat_map (fun path ->
      extract_cleanup_file path
        ?cleanup_to:(if cleanup then Some path else None))

let cleanup_files paths =
  extract_cleanup_files ~cleanup:true paths |> ignore

let grade_files ?points_step_count ?cleanup grading paths =
  extract_cleanup_files ?cleanup paths |> List.concat
  |> evaluate_grading ?points_step_count grading


let align_tabs text =
  let[@tail_mod_cons] rec drop_last = function
    | [] | [ _ ] -> []
    | x :: xs -> x :: drop_last xs
  in
  let last xs = match List.rev xs with [] -> None | x :: _ -> Some x in
  let[@tail_mod_cons] rec combine ?fb1 ?fb2 f xs ys =
    match xs, fb1, ys, fb2 with
    | x :: xs, _, y :: ys, _ -> f x y :: combine ?fb1 ?fb2 f xs ys
    | [], Some fb, _, _ -> List.map (fun y -> f fb y) ys
    | _, _, [], Some fb -> List.map (fun x -> f x fb) xs
    | _ -> []
  in
  let combine_fb f l1 l2 fb1 fb2 = combine ~fb1 ~fb2 f l1 l2 in
  let lines =
    text |> String.split_on_char '\n'
    |> List.map (String.split_on_char '\t')
  in
  let indents =
    lines |> List.map (List.map String.length)
    |> List.fold_left (fun acc v -> combine_fb max acc (drop_last v) 0 0) []
  in
  let align_line line =
    let f str indent = str ^ String.make (indent - String.length str) ' ' in
    combine f ~fb2:0 (drop_last line) indents @ Option.to_list (last line)
  in
  lines |> List.map align_line
  |> List.map (String.concat "") |> String.concat "\n"

let mk_points count ~passed =
  let open Junit.Testcase in
  List.init count @@ fun n ->
      let name = Printf.sprintf "points:%d" n in
      let mk = if n < passed then pass else failure "" ~typ:"points" in
      mk ~name ~classname:"grading" ~time:0.

let testsuite_of_result result =
  let open Junit in
  let points = mk_points result.max_points ~passed:result.points in
  let feedback =
    Testcase.failure result.text ~name:"feedback"
      ~typ:"feedback" ~classname:"grading" ~time:0.
  in
  let open Testsuite in
  make ~name:"grading" ()
  |> add_testcases (feedback :: points)

let write_result result path =
  let text = align_tabs result.text in
  (* TODO: make error output configurable? *)
  List.iter prerr_endline [ String.make 78 '='; text; String.make 78 '-' ];
  let ts = testsuite_of_result { result with text } in
  Junit.to_file (Junit.make [ts]) path

let grade_files_to ?points_step_count ?cleanup ~grading_to grading paths =
  let result = grade_files ?points_step_count ?cleanup grading paths in
  write_result result grading_to

let grade_cleanup_files_to ?points_step_count ~grading_to ?grading =
  match grading with
  | None -> cleanup_files
  | Some g -> grade_files_to ?points_step_count ~cleanup:true ~grading_to g

(* compat *)
let prettify_results ?grading path =
  grade_cleanup_files_to [path] ?grading
    ~grading_to:Filename.(concat (basename path) "grading.xml")

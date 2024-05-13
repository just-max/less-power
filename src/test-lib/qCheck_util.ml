(** Helpers for QCheck-based tests. *)

open Common

let default_computation_timeout = Mtime.Span.(100 * ms)

(** Workaround to check if an exception is [QCheck2.Test.User_fail],
    since that exception is not exposed. *)
let is_user_fail exn =
  Scanf.ksscanf
    (Printexc.to_string_default exn)
    (fun _ _ -> None)
    "QCheck2.Test.User_fail(%S)" Option.some

(** Check if values are equal. Report a failure message as [Error] if not.
    This monadic form is useful for properties that check nested functions,
    i.e. calling submission code contains functions that should once again
    be called.

    Note: with [timeout], uses {!Common.Util.timeout_unix}, and thus nesting
    (within [expected_f] or [actual_f]) will not work as expected.

    @param context Add this string as context before failure message
    @param eq Check for equality, return [Some x] if equal or [None] if not
    @return The result of [eq], if not [None], as [Ok]. *)
let assert_equal
    ?context ?(timeout = default_computation_timeout) ~eq ~printers
    expected_f actual_f =
  let open Result in
  let open Ctx_util in

  let report msg =
    match context with None -> msg | Some c -> c ^ "\n" ^ msg
  in

  let wrap f =
    let< () = capture_exceptions () in
    let< () = timeout_unix timeout in
    f ()
  in

  let expected =
    let error_msg =
      "The solution raised an error, please report this to an instructor"
    in
    match wrap expected_f with
    | Ok (Some x) -> Ok x
    | Ok None | Error _ -> Error (report error_msg)
  in

  let actual =
    let error_msg d = report ("Your submission raised an error: " ^ d) in
    (* Ok Some: all good; Ok None: timeout; Error e: exception raised *)
    match wrap actual_f with
    | Ok (Some x) -> Ok x
    | Ok None -> Error (report @@ "Your submission timed out")
    | Error e ->
        match is_user_fail e with
        | Some s -> Error (report s)
        | None -> Error (error_msg @@ Printexc.to_string e)
  in

  let ( let* ) = bind in
  let* e = expected in
  let* a = actual in
  match eq e a with
  | Some ea -> Ok ea
  | None ->
      String.concat "\n"
        [ "Expected:"; fst printers e; "But got:"; snd printers a ]
      |> report |> Result.error

(** Turn an equality function into the form required for {!assert_equal}. *)
let lift_eq eq x y = if eq x y then Some () else None

(** Like {!assert_equal}, but when the types to compare are the same. *)
let assert_equal' ?(eq = lift_eq (=)) ~printer =
  assert_equal ~eq ~printers:(printer, printer)

let fail_reportf_notrace fmt =
  Format.kdprintf
    (fun pp ->
      Printexc.record_backtrace false;
      QCheck2.Test.fail_reportf "%t" pp)
    fmt

let fail_report_notrace s =
  Printexc.record_backtrace false;
  QCheck2.Test.fail_report s

(** After performing one or more comparisons with {!assert_equal},
    report a failure as a QCheck failure, or just return [true] otherwise. *)
let report_result = function
  | Ok _ -> true
  | Error e -> fail_report_notrace e

(** Check a property against multiple, fixed inputs. *)
let make_test_singles ?name ?(print : 'a QCheck2.Print.t option) xs prop =
  let lift f x = f (x ()) in
  let open QCheck2 in
  Test.make ?name ?print:(Option.map lift print) ~count:(List.length xs)
    Gen.(graft_corners (pure (fun () -> failwith "")) (List.map Fun.const xs) ())
    (lift prop)

(** Check a property against a fixed input. *)
let make_test_single ?name ?print x prop =
  make_test_singles ?name ?print [ x ] prop

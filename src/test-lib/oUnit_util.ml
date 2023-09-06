(** Utilities for converting from QCheck to OUnit,
    and other OUnit-related functionality. *)

open OUnit2
open OUnitTest
open Common

let default_qcheck_timeout = Mtime.Span.(5 * s)

let default_rand =
  (* random seed, for repeatability of tests *)
  Random.State.make [| 89809344; 994326685; 290180182 |]
(* TODO: maybe a better mechanism can be used here *)

(** Convert a QCheck test to OUnit, overwriting the timeout.
    Automatically prevents excessive shrinking from timing out tests,
    by interrupting shrinking and showing the smallest value found so far
    once the timeout expires. *)
let of_qcheck ?(timeout = default_qcheck_timeout) (QCheck2.Test.Test cell) =
  Printexc.record_backtrace true;
  let module T = QCheck2.Test in
  let name = T.get_name cell in
  let count = T.get_count cell in
  let test_fun _ =
    let start = Mtime_clock.counter () in
    let passed = ref 0 in
    let last_shrunk = ref None in
    let rand = Random.State.copy default_rand in
    let step _ _ _ = function[@warning "-4"]
      | T.Success -> incr passed
      | _ -> ()
    in
    let cancel_test time = function
      | Some v ->
          T.make_cell ~count:1 ?print:(T.get_print_opt cell)
            (QCheck2.Gen.pure v) (T.get_law cell)
          |> T.check_cell_exn
      | None ->
          Format.asprintf "test `%s` passed %d of %d required checks in %a"
            name !passed count Mtime.Span.pp time
          |> assert_failure
    in
    let handler _ _ event =
      (match[@warning "-4"] event with
      | T.Shrunk (_, v) -> last_shrunk := Some v
      | _ -> ());
      let time = Mtime_clock.count start in
      if Mtime.Span.compare time timeout > 0 then cancel_test time !last_shrunk
    in
    T.check_result cell @@ T.check_cell cell ~rand ~step ~handler
  in
  (* timeout * 1.25: arithmetic not built-in to mtime *)
  let test_length =
    let open Mtime.Span in
    let timeout_ns = to_uint64_ns timeout in
    let extra = Int64.(div timeout_ns (of_int 4)) |> of_uint64_ns in
    Custom_length (add timeout extra |> Util.span_to_float_s)
  in
  name >: TestCase (test_length, test_fun)


(** Visibility of tests after hiding. *)
type visibility =
  | PassFail
      (** Test is always run, but the detailed result is not shown. Instead, a
          failure is indicated with a generic message. Also known as "secret"
          tests. *)
  | None
      (** Test is not run. Instead, the test fails and a generic message
          indicates that the test was not run. Also known as "hidden" tests. *)

let message_of_visibility = function
  | PassFail -> "This secret test has failed."
  | None -> "This hidden test was not executed."

(** Hide a test according to the [visibility] argument. *)
let hide_test ?(visibility = PassFail) =
  let fail_test () =
    raise_notrace (OUnit_failure (message_of_visibility visibility))
  in
  let hide_func func ctx =
    match visibility with
    | PassFail -> (try func ctx with _ -> fail_test ())
    | None -> fail_test ()
  in
  let rec hide = function
    | TestCase (dur, func) -> TestCase (dur, hide_func func)
    | TestList tests -> TestList (List.map hide tests)
    | TestLabel (name, inner) -> TestLabel (name, hide inner)
  in
  hide


(** Join a test tree into a single test which only passes if all individual
    tests in the tree pass. Students will only be able to see the result of
    the first test that failed. Timeouts of individual tests are summed to
    produce a single timeout, which is applied to the combined test. This
    changes the timeout behavior of the tests. Intended for grouping tests,
    when grading is performed solely by assigning points to passed tests.

    For more complex grouping, use the {!module-Grading} module. *)
let join_tests test =
  let rec collect (timeout, tests) = function
    | TestList list -> List.fold_left collect (timeout, tests) list
    | TestLabel (_, inner) -> collect (timeout, tests) inner
    | TestCase (t, f) -> (timeout +. delay_of_length t, f :: tests)
  in let (t, f) = collect (0.0, []) test in
  TestCase (Custom_length t, fun ctx -> List.iter (( |> ) ctx) f)

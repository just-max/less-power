(** Provide a grade from a list of passed/failed tests and a grading scheme.
    Provides functionality to load tests from a JUnit XML file,
    tidy them up, and write the grading result back to a JUnit XML file. *)

(** {1 Grading} *)

type 'a clamp = { min : 'a; max : 'a }
(** A range of values, inclusive. *)

val clamp : 'a clamp -> 'a -> 'a
(** [clamp { min = x; max = y } z] constrains [z] between [x] and [y]. *)

val clamp_opt : 'a clamp option -> 'a -> 'a
(** [clamp None x] is [x], otherwise as {!val-clamp}. *)

type grading_result = { text : string; points : int; max_points : int }
(** The result of grading a list of tests. *)

type tests = (string * bool) list
(** Pairs of [test_name, passed]. *)

type testsuites = tests list
(** A group of testsuites (e.g. as extracted from a single JUnit XML file) *)

(** Logical formulas over test results. *)
type grading_criterion =
  | Passed of string  (** Did the given test pass? *)
  | Failed of string  (** Did the given test fail? *)
  | Not of grading_criterion  (** Negation (logical not). *)
  | OneOf of grading_criterion list  (** Disjunction (logical or). *)
  | AllOf of grading_criterion list  (** Conjunction (logical and). *)
  | Constant of bool  (** Logical constant. *)
(** [Passed] and [Failed] values may use wildcards:

  - Trailing colon, e.g. [a:b:c:]: matches anything of which it is a prefix,
      e.g. [a:b:c:d:e] and [a:b:c].
  - Star as name, e.g. [a:*:c]: matches any name in place of [*],
      e.g. [a:mmm:c].

  Both may be combined, e.g. [a:*:c:] matches [a:mm:c] and [a:nn:c:d]. A name
  is considered to be any string that does not contain a [:]. You are
  responsible for enforcing this convention yourself, it is not enforced
  by OUnit, for example.

  For a [Passed] value to evaluate to true, {i all} tests matched by the
  wildcard need to pass, and for [Failed] all need to fail. If you want the
  inverse behavior ({i at least} one matched test needs to pass/fail),
  use [Not]: [Not (Failed "a:*:")] means "not all matched tests failed",
  which is logically equivalent to "at least one matched test passed".
  For this, you can use {!any_passed} and {!any_failed}.

  It is an error to reference a test that does not exist.
  In the presence of wildcards, at least one test must match.
*)

val implies : grading_criterion -> grading_criterion -> grading_criterion
(** Logical implication: [implies a c = OneOf [Not a; c]]. *)

val any_passed : string -> grading_criterion
(** If a test case contains a wildcard, the behavior is to require that all
    tests that match pass (or that all tests fail). Use [any_passed]
    (and [any_failed]) to instead check that at least one test that matches
    passed (or at least one test that matches failed). *)

val any_failed : string -> grading_criterion
(** See {!any_passed}. *)

val evaluate_criterion : tests -> grading_criterion -> bool
(** Evaluate the logical formula over the assignment
    of test names to boolean pass/fail values. *)

val string_of_grading_criterion : ?indent:int -> grading_criterion -> string

(** Grading scheme. Defines how to assign a numeric grade for a given set of test results. *)
type grading =
  | Points of {
      title : string;
      points : int;
      test_case : grading_criterion;
      reason : tests -> grading_criterion -> string;
    }
      (** If the criterion [test_case] evaluates to [true],
           assign a grade of [points], otherwise [0]. *)
  | Group of {
      title : string;
      items : grading list;
      max_points : int clamp option;
      skip : string option;
          (** If [skip = Some r], skip this group (assign a grade of [0]) with the reason [r]. *)
    }
      (** A group of grading schemes. Assign the sum of grades from the sub-items,
           with an optional minimum/maximum number of points. *)
  | Conditional of {
      condition : grading_criterion;
      message : string;
      content : grading;
    }
      (** Assign a grade according to [content] if the [condition] evaluates to [true], otherwise [0]. *)

exception No_reason

(** Wrapper for {{!type-grading.Points}[Points]}. If [reason] returns [None] (the default),
    the displayed reason is decided by [penalty] and [points]:

    - ["PASS"]/["FAIL"] if [points] is non-negative, or [penalty] is [true]
    - If [points] is negative, or [penalty] is [false], then the grading criterion must
      evaluate to [true] and the reason is ["PENALTY"], otherwise {!No_reason} is raised
      during evaluation.

    If [reason] returns a value, that is used directly instead. *)
val points :
  ?skip:grading_criterion ->
  ?reason:(tests -> grading_criterion -> string option) ->
  ?penalty:bool ->
  string ->
  int ->
  grading_criterion ->
  grading

(** Wrapper for {{!type-grading.Points}[Points]}, intended for debugging tests.
    If the given {!grading_criterion} does not evaluate to true,
    the [message] is given as a reason along with a print-out of
    the criterion. Useful for verifying invariants between tests, e.g. that passing
    a more comprehensive test implies passing a test that tests a subset of functionality. *)
val assertion :
  ?message:string -> ?title:string -> int -> grading_criterion -> grading

val points_p :
  ?skip:grading_criterion ->
  ?reason:(tests -> grading_criterion -> string option) ->
  ?penalty:bool ->
  string ->
  int ->
  string ->
  grading

val points_f :
  ?skip:grading_criterion ->
  ?reason:(tests -> grading_criterion -> string option) ->
  ?penalty:bool ->
  string ->
  int ->
  string ->
  grading

val points_c :
  ?skip:grading_criterion ->
  ?reason:(tests -> grading_criterion -> string option) ->
  ?penalty:bool ->
  string ->
  int ->
  bool ->
  grading
(** As per {!val-points}, but with the grading criterion set to
    {{!type-grading_criterion.Passed}[Passed]}, {{!type-grading_criterion.Failed}[Failed]},
    or {{!type-grading_criterion.Constant}[Constant]}, respectively. *)

val conditional : grading_criterion -> string -> grading -> grading
val conditional_c : bool -> string -> grading -> grading
(** Wrappers for {{!type-grading.Conditional}[Conditional]}, with
    the given condition or a constant. *)

val group :
  ?skip:string -> ?max_points:int clamp -> string -> grading list -> grading
(** Wrapper for {{!type-grading.Group}[Group]}. *)

val evaluate_grading :
  ?points_step_count:int -> grading -> tests -> grading_result
(** Calculates a grade from the description of the
    grading and the list of passed/failed tests. *)

(** {1 Reading and writing}

    Functionality to read tests from JUnit XML files, clean up the files,
    and write evaluated grading to JUnit XML files. *)

(** {2 Reading and clean up} *)

val extract_cleanup_file : ?cleanup_to:string -> string -> testsuites
(** Extract the list of testsuites from a JUnit XML file. If [cleanup_to] is
    given, write the result of tidying up the result back to disk. The paths
    may be identical, in which case the original file is overwritten. *)

(** {2 Writing} *)

val write_result : ?log_result:bool -> ?points_step_count:int -> grading_result -> string -> unit
(** Write a grading result to file. If [log_result] is [true] (default),
    print a human-readable summary of grading results to standard error.
    If [points_step_count] is given, it is used in the error
    message of the generated [points] tests. *)

(** {2 Helpers} *)

(** Helpers to perform the most common sequence of operations:

    + Read a list of JUnit XML files, extracting their test suites,
      and (optionally) cleaning them up, in-place.
    + Evaluate the list of passing/failing tests on a given grading scheme.
    + Write the result to file.

    Steps 2 and 3 can be skipped if no grading scheme is used.
    These helpers always clean up in-place, i.e. overwrite
    the original XML file, which is the intended use. *)

val extract_cleanup_files : ?cleanup:bool -> string list -> testsuites
(** As {!extract_cleanup_file} but for a list of files. The list of testsuites
    from each file are concatenated to produce a single list of testsuites.
    If [cleanup] is [true] (default), then overwrite each file with
    the cleaned up result, otherwise don't clean up. *)

val process_files :
  ?cleanup:bool -> ?grade:(grading * int option * string) ->
  ?log_result:bool -> string list -> unit
(** Combines {!extract_cleanup_files}, {!evaluate_grading} and {!write_result}.

    Extracts grading and (optionally) cleans up in-place,
    as per {!extract_cleanup_files}.
    If [grade] is [Some (grading, points_step_count, path)],
    then evaluate [grading] as per {!evaluate_grading} with the given
    [points_step_count], and write the result to [path],
    as per {!write_result}. *)

val grading_options :
  ?points_step_count:int ->
  grading_to:string ->
  grading ->
  grading * int option * string
(** Helper to write the [grade] argument to {!process_files} more nicely. *)

val[@deprecated "use process_files"] prettify_results :
  ?grading:grading -> string -> unit
(** As per {!process_files} with a single path and
    [~cleanup:true]. If [grading] is given, then that grading is passed through
    with a [point_step_count] of [1] and the grading result written to
    [grading.xml] in the same directory as the given file. *)

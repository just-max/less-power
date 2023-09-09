(** Provide a grade from a list of passed/failed tests and a grading scheme. *)

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

  Both may be combined, e.g. [a:*:c:] matches [a:mm:c] and [a:nn:c:d].

  For a [Passed] value to evaluate to true, **all** tests matched by the
  wildcard need to pass, and for [Failed] all need to fail. If you want the
  inverse behavior ( **at least** one matched test needs to pass/fail),
  use [Not]: [Not (Failed "a:*:")] means "not all matched tests failed",
  which is logically equivalent to "at least one matched test passed".

  It is in error to reference a test that does not exist.
  In the presence of wildcards, at least one test must match.
*)

val implies : grading_criterion -> grading_criterion -> grading_criterion
(** Logical implication: [implies a c = OneOf [Not a; c]]. *)


(* val mk_indent : int -> string *)
val string_of_grading_criterion : ?indent:int -> grading_criterion -> string
(* val eval_criterion : tests -> grading_criterion -> bool *)

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

(** Main entry point: [prettify_results ~grading file_name] reads the JUnit XML
    file in [file_name], evalutes the given [grading] scheme and writes the result
    back to the same file. *)
val prettify_results :
  ?grading:grading -> ?points_step_count:int -> string -> unit

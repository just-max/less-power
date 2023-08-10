(** Context managers, loosely inspired by Python *)

(** A context manager of type [('a, 'b, 'c) t] takes a continuation, and
    should feed the continuation a value of type ['a]. Once the continuation
    returns with either a [Ok of 'b] or an exception, the continuation should
    perform cleanup, and may suppress the exception by producing a suitable
    result (of type ['c]) instead. Typically, ['b = 'c].

    This representation has the advantage that some existing functions library
    already implement this type (e.g. {!In_channel.with_open_text}). *)
type ('a, 'b, 'c) t = ('a -> ('b, exn) result) -> ('c, exn) result

(** [with_context cm f] runs [f] in the context manager [cm] *)
let with_context (cm : _ t) f =
  let k x =
    try Ok (f x)
    with e -> Error e
  in
  match cm k with
  | Ok x -> x
  | Error e -> raise e

(** Let-syntax for {!with_context} *)
let ( let< ) = with_context

(* {1 Context managers} *)

(** As per {!Filename.temp_file}. Removes the temporary file upon completion. *)
let temp_file ?temp_dir prefix suffix : _ t = fun k ->
  let path = Filename.temp_file ?temp_dir prefix suffix in
  let result = k path in
  Sys.remove path;
  result

(** As per {!Unix.openfile}. Closes the file descriptor upon completion. *)
let openfile path flags perm : _ t = fun k ->
  let fd = Unix.openfile path flags perm in
  let result = k fd in
  Unix.close fd;
  result

(** As per {!Sys.set_signal}. Restores the previous signal behavior on completion. *)
let set_signal signal behavior : _ t = fun k ->
  let prev_behavior = Sys.signal signal behavior in
  let result = k () in
  Sys.set_signal signal prev_behavior;
  result

(** As per {!Unix.sigprocmask}. Restores the previous mask on completion. *)
let sigprocmask mode sigs : _ t = fun k ->
  let prev_mask = Unix.sigprocmask mode sigs in
  let result = k () in
  Unix.sigprocmask Unix.SIG_SETMASK prev_mask |> (ignore : int list -> _);
  result

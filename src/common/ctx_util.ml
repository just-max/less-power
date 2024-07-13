(** Context managers, loosely inspired by Python *)

(** Exception and its associated (raw) backtrace. *)
type exn_info = {
  exn : exn ;
  backtrace : Printexc.raw_backtrace ;
}

(** A context manager of type [('a, 'b, 'c) t] takes a continuation, and
    should feed the continuation a value of type ['a]. Once the continuation
    returns with either a [Ok of 'b] or an {!exn_info}, the continuation should
    perform cleanup, and may suppress the exception by producing a suitable
    result (of type ['c]) instead. Often, ['b = 'c].

    This representation has the advantage that some existing functions library
    already implement this type (e.g. {!In_channel.with_open_text}). *)
type ('a, 'b, 'c) t = ('a -> ('b, exn_info) result) -> ('c, exn_info) result

(** [with_context cm f] runs [f] in the context manager [cm] *)
let with_context (cm : _ t) f =
  let k x =
    try Ok (f x)
    with exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      Error { exn; backtrace }
  in
  match cm k with
  | Ok x -> x
  | Error { exn ; backtrace } ->
      Printexc.raise_with_backtrace exn backtrace

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

let timeout_unix ?timer t : _ t = fun k ->
  match Util.timeout_unix ?timer t k () with
  | None -> Ok None
  | Some r -> Result.map Option.some r

let timed : _ t = fun k ->
  match Util.timed k () with
  | Error _ as e, _ -> e
  | Ok r, t -> Ok (r, t)

let lock_mutex m : _ t = fun k ->
  let _ = Mutex.lock m in
  let result = k () in
  let _ = Mutex.unlock m in
  result

let capture_exceptions ?(filter = Fun.const true) () : _ t = fun k ->
  match k () with
  | Ok x -> Ok (Ok x)
  | Error exn_info when filter exn_info.exn -> Ok (Error exn_info)
  | Error exn_info -> Error exn_info

let empty_context x f : _ t = fun k -> Result.map f (k x)
let empty_context' x : _ t = empty_context x Fun.id

let optional_context ~some x_empty f_empty = function
  | None -> empty_context x_empty f_empty
  | Some x -> some x

let optional_timeout_unix ?timeout =
  optional_context ~some:timeout_unix () Option.some timeout

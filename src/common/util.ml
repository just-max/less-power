(** Various utility functions. *)

(** Function composition, [f % g] is {m f \circ g}. *)
let (%) f g x = f (g x)

(** Reverse function composition, [f %> g] is {m g \circ f}. *)
let (%>) f g x = g (f x)

(** [peek f x] is [(f x; x)]. *)
let peek f x = f x; x

(** One-element list. *)
let singleton x = [x]

let or_option o1 o2 = o1 |> Option.(fold ~some ~none:o2)

(** [string_contains ~needle haystack]:
    does [needle] exist as a substring of [haystack]?
    Naive {m \mathcal{O}(nm) } implementation. *)
let string_contains ~needle haystack =
  Seq.ints 0
  |> Seq.take String.(max 0 @@ length haystack - length needle + 1)
  |> Seq.map (fun i -> String.sub haystack i (String.length needle))
  |> Seq.exists ((=) needle)

(** [(~$ x) f] is [f x], e.g.:
    [List.map (~$ 2) [List.nth ["a"; "b"; "c"]; String.sub "def" 1]]
    returns [["c"; "ef"]] *)
let ( ~$ ) = ( |> )

let try_to_result f x = try Ok (f x) with e -> Error e
let unresult_exn = function Ok x -> x | Error e -> raise e

let run_main main =
  Sys.argv
  |> Array.to_list
  |> (function cmd :: args -> main cmd args | [] -> failwith "empty argv")
  |> exit

(** {1 Timeouts} *)

let min_span s1 s2 = if Mtime.Span.compare s1 s2 < 0 then s1 else s2

exception Timeout
let _ = Printexc.register_printer (function Timeout -> Some "Timeout" | _ -> None)

(** Can overflow for large [t] *)
let span_to_float_s t = Mtime.Span.(to_float_ns t /. to_float_ns s)

(** [timeout_unix t f] is [Some (f ())] if the call to [f]
    terminates within [t], otherwise [None].
    The timeout is triggered by a Unix signal.

    Notes:
    - [-] Does not interrupt [f] until an allocation occurs, which may be never.
    - [-] Any existing Unix timer is suspended for the duration of the call to [f].
    - [+] If [f] is interrupted, it will not be left running in the background. *)
let timeout_unix ?(timer = Unix.ITIMER_REAL) t f x =
  let open Unix in
  let signum = match timer with
    | ITIMER_REAL -> Sys.sigalrm
    | ITIMER_VIRTUAL -> Sys.sigvtalrm
    | ITIMER_PROF -> Sys.sigprof
  in
  let stop_timer () = Unix.setitimer timer { it_value = 0.; it_interval = 0. } in

  let t_float = span_to_float_s t in

  let prev_timer = stop_timer () in
  let prev_handler = Sys.signal signum (Sys.Signal_handle (fun _ -> raise_notrace Timeout)) in

  let r =
    try
      Unix.setitimer timer { it_value = t_float; it_interval = 0. } |> ignore ;
      let r0 = try Ok (Some (f x)) with e when e <> Timeout -> Error e in
      stop_timer () |> ignore;
      r0
    with Timeout -> Ok None
  in

  Sys.set_signal signum prev_handler ;
  Unix.setitimer timer prev_timer |> ignore ;

  unresult_exn r

let timed f x =
  let c = Mtime_clock.counter () in
  let r = f x in
  let t = Mtime_clock.count c in
  r, t

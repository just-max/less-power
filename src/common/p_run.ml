(** Even higher-level wrapper around [Unix.open_process]. *)

(** Waits for the given process or times out. *)
let wait_pid_timeout t pid =
  let open Ctx_util in
  let< () = sigprocmask Unix.SIG_BLOCK [Sys.sigchld] in

  let exception Sigchld in
  let< () = set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> raise Sigchld)) in
  let t0 = Mtime_clock.counter () in
  let rec task () =
    let pidw, status = Unix.(waitpid [WNOHANG] pid) in
    if pidw <> 0 then Some status
    else
      try
        let< () = sigprocmask Unix.SIG_UNBLOCK [Sys.sigchld] in
        let elapsed = Mtime_clock.count t0 in
        if Mtime.Span.compare elapsed t < 0 then
          Unix.sleepf @@ Util.span_to_float_s @@ Mtime.Span.abs_diff elapsed t;
        None
      with
        Sigchld -> task ()
  in
  task ()

(** Timeout after [duration], sending [signal]. If [kill_after] is [Some t],
    then send [SIGKILL] after waiting an additional [t]. *)
type timeout_description =
  { duration : Mtime.span; signal : int; kill_after : Mtime.span option }

let timeout ?(signal = Sys.sigterm) ?kill_after duration =
  { duration; signal; kill_after }

type phase = Completed | TimedOut | Killed
(** Whether the process is running normally, has received
    the first signal, or has received a SIGKILL. *)

type result = {
    phase : phase (** What phase was the process in when it completed? *);
    status : Unix.process_status (** What status did the process exit with? *);
    stdout : string;
    stderr : string;
    elapsed : Mtime.span;
  }

(** Did the subprocess exit normally? If [check_status] is [false], ignore
    the exit code and check only for a timeout. *)
let result_is_ok ?(check_status = true) = function[@warning "-4"]
  | { phase = Completed; _ } when not check_status -> true
  | { phase = Completed; status = Unix.WEXITED 0; _ } -> true
  | _ -> false

let pp_result ?(hide_stdout = false) ?(hide_stderr = false) ?command_line ppf r =
  let open Fmt in
  let open Pp_util in
  let pp_status ppf = function
    | TimedOut, _ -> string ppf "timed out"
    | Killed, _ -> string ppf "timed out and was killed"
    | Completed, Unix.WEXITED st -> pf ppf "exited with status %d" st
    | Completed, Unix.WSIGNALED si -> pf ppf "exited on signal %a" Dump.signal si
    | Completed, Unix.WSTOPPED si -> pf ppf "stopped on signal %a" Dump.signal si
  in
  let pp_output name ppf body =
    let w = 40 in
    let wn = String.length name + 2 in
    let we1 = (w - wn) / 2 in
    let we2 = w - we1 - wn in
    pf ppf "@[<v>%a %s %a@,%a%a@]"
      (pp_repeat we1 char) '=' name (pp_repeat we2 char) '='
      pp_text_ body
      (pp_repeat w char) '='
  in
  pf ppf "@[<v>@[%a@ %a@ after %a@]%a%a@]"
    (option
      ~none:(const string "A command")
      (fun ppf -> pf ppf "@[<2>The command `@[<2>%a@]`@]" (list ~sep:sp @@ pp_of string (* Filename.quote *) Fun.id)))
    command_line
    pp_status (r.phase, r.status)
    Mtime.Span.pp r.elapsed
    (option (cut ++ pp_output "STDOUT"))
    (if hide_stdout then None else Some r.stdout)
    (option (cut ++ pp_output "STDERR"))
    (if hide_stderr then None else Some r.stderr)

(** Send the [input] to [command] with the given [args].
    Note: uses temporary files provided by {!Filename}. *)
let p_run ?timeout:timeout_desc ?input ?(args = []) command =
  let open Ctx_util in

  let< stdout_path = temp_file command "stdout" in
  let< stderr_path = temp_file command "stderr" in

  let phase, status, elapsed =
    let< stdin_path = temp_file command "stdin" in
    input |> Option.iter (fun inp ->
        let< stdin_ch = Out_channel.with_open_text stdin_path in
        let ppf = Format.formatter_of_out_channel stdin_ch in
        Format.fprintf ppf "%t" inp;
        Format.pp_print_flush ppf ());

    let< stdin_fd  = openfile stdin_path  Unix.[O_RDONLY] 0o000 in (* ; O_CLOEXEC? *)
    let< stdout_fd = openfile stdout_path Unix.[O_WRONLY] 0o000 in
    let< stderr_fd = openfile stderr_path Unix.[O_WRONLY] 0o000 in

    let t0 = Mtime_clock.counter () in

    let pid =
      Unix.create_process command
        (Array.of_list (command :: args))
        stdin_fd stdout_fd stderr_fd
    in

    let rec timeouts phase = function
      | [] -> phase, snd (Unix.waitpid [] pid)
      | (t, signal, phase') :: ts ->
          match wait_pid_timeout t pid with
          | Some status -> phase, status
          | None ->
              Unix.kill pid signal;
              timeouts phase' ts
    in
    let phase, status =
      timeouts Completed @@
      match timeout_desc with
      | None -> []
      | Some { duration; signal; kill_after = None } ->
          [(duration, signal, TimedOut)]
      | Some { duration; signal; kill_after = Some t } ->
          [(duration, signal, TimedOut); (t, Sys.sigkill, Killed)]
    in
    let elapsed = Mtime_clock.count t0 in
    phase, status, elapsed
  in

  let input_all path =
    let< ch = In_channel.with_open_text path in
    In_channel.input_all ch
  in
  let stdout = input_all stdout_path in
  let stderr = input_all stderr_path in

  { phase; status; stdout; stderr; elapsed }

(** Like {!Stdlib_alerts}, but for threads.
    This is separate to avoid an unnecessary dependency on the [threads] library *)

module type Thread_alerting = sig

  [%%include
    (* [exit] is deprecated, but not harmful *)
    threads.thread (t, create, self, id, Exit, exit, delay, join, yield, default_uncaught_exception_handler),
    { attributes = __ [@alert unsafe "This function is not permitted"];
      items = threads.thread (wait_timed_read, wait_timed_write, select, wait_pid, sigmask, wait_signal) },
    { attributes = __ [@alert impure "This imperative programming function is not permitted"];
      items = threads.thread set_uncaught_exception_handler }
  ]

end

module type Event_alerting = sig [%%include threads.event !standard] end


module Thread_alerting : Thread_alerting = Thread
module Event_alerting : Event_alerting = Event

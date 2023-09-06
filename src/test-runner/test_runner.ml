(** Library for creating a runner, which then drives the testing process. *)

module Task = Task
module Std_task = Std_task
module Entry_point = Entry_point

(** Re-export of {!Common.P_run}. Tasks spawning
    subprocesses can use parts of this module. *)
module P_run = Common.P_run

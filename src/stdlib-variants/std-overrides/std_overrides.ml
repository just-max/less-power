(** Ready-to-use modules for replacing the default pervasives, using [-open]. *)

module Hide_stdlib_variants = struct
  (** Prevent access to the full variant library. Intended to be opened at the top level
      to hide the [Stdlib_variants] module, and a couple others.
      
      This is included in the other overrides modules, so you usually don't need
      to pass it to the [-open] flag yourself. However, you should use it
      if you define your own override module based on less-power stdlib variants. *)

  (** {i These modules are intentionally empty.} *)

  module Stdlib_variants = struct end

  (* Shouldn't be necessary with (implicit_transitive_deps false), but to be safe... *)
  module Stdlib_components = struct end
  module Stdlib_alerts = struct end
  module Threads_alerts = struct end

end


(** Puts the safe parts of [Stdlib] into scope,
    from {!Stdlib_components.Stdlib_safe} *)
module Stdlib_safe = struct

  module Stdlib = Stdlib_components.Stdlib_safe
  include Stdlib_components.Stdlib_safe

  include Hide_stdlib_variants
end

(** Puts an alerting [Stdlib] into scope,
    from {!Stdlib_alerts.Stdlib_alerting}. *)
module Stdlib_alerting = struct

  module Stdlib = Stdlib_alerts.Stdlib_alerting
  include Stdlib_alerts.Stdlib_alerting

  include Hide_stdlib_variants
end

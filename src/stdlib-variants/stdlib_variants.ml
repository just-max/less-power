(** Variants of [Stdlib] with restricted or modified functionality. *)


(** {!modules: Stdlib_components} *)

module Stdlib_components = Stdlib_components

(** {!modules: Stdlib_alerts} *)

module Stdlib_alerts = Stdlib_alerts

module Hide_stdlib_variants = struct
  (* Prevent access to the full variant library. *)
  module Stdlib_variants = struct end

  (* Shouldn't be necessary with (implicit_transitive_deps false), but to be safe... *)
  module Stdlib_components = struct end
  module Stdlib_alerts = struct end
end

module Stdlib_safe = struct
  (** [SAFE] Everything safe from the standard library, including
      everything safe from sub-modules of [Stdlib].*)

  open Stdlib_components

  include Exceptions
  include Composition
  include Debugging
  include Comparisons
  include BooleanOperations
  include IntegerOperations
  include FloatingPointOperations
  include StringOperations
  include CharOperations
  include UnitOperations
  include PairOperations
  include Result
  include StringConversion
  include ListOperations
  include Formats

  include SafeAliases

  include Hide_stdlib_variants
end

module SafeStdlib = Stdlib_safe (* compat *)

module Stdlib_alerting = struct
  include (Stdlib : Stdlib_alerts.Stdlib_alerting)
  include Hide_stdlib_variants
end

(** Variants of [Stdlib] with restricted or modified functionality. *)


(** {!modules: Stdlib_components} *)

module Stdlib_components = Stdlib_components

(** {!modules: Alert_stdlib} *)

module Alert_stdlib = Alert_stdlib

module Hide_stdlib_variants = struct
  (* Prevent access to the full variant library. *)
  module Stdlib_variants = struct end
end

module SafeStdlib = struct
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

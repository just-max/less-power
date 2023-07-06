(** Variants of [Stdlib] with restricted or modified functionality. *)

module Stdlib_components = Stdlib_components

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
end

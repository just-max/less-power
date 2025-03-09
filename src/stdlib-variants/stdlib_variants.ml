open Std_overrides


(* compat *)
module SafeStdlib = struct
  include Stdlib_safe
  include Hide_stdlib_variants
end
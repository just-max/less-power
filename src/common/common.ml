(** Functionality common to multiple components. *)

module Error_context = Error_context

module Internal = struct
  [@@@alert lp_internal "This module is internal and has an unstable interface!"]

  module Util = Util
  module Path_util = Path_util
  module Syntax = Syntax
end

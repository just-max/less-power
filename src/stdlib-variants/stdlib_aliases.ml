(** TODO: module doc *)

module SafeAliases = struct
  (** [SAFE] Safe modules from the standard library, and
      unsafe modules restricted to their safe functions. *)

  (* Whenever we change the definition of a library Foo, we add a line
      module _ = (Stdlib.Foo : module type of Foo)
      to check that our Foo module is a subset of the stdlib Foo *)

  open Stdlib
  (* module Arg            = Arg *)
  (* module Array          = Array *)
  (* module ArrayLabels    = ArrayLabels *)
  (* module Atomic         = Atomic *)
  (* module Bigarray       = Bigarray *)
  module Bool           = Bool
  (* module Buffer         = Buffer *)
  (* module Bytes          = Bytes *)
  (* module BytesLabels    = BytesLabels *)
  (* module Callback       = Callback *)
  module Char           = Char
  module Complex        = Complex
  (* module Condition      = Condition *)
  (* module Digest         = Digest *)
  (* module Domain         = Domain *)
  (* module Effect         = Effect *)
  module Either         = Either
  (* module Ephemeron      = Ephemeron *)
  (* module Filename       = Filename *)
  module Float          = (Float : Sig_float.SafeFloat)
  (* module Format         = Format *)
  module Fun            = Fun
  (* module Gc             = Gc *)
  (* module Hashtbl        = Hashtbl *)
  (* module In_channel     = In_channel *)
  module Int            = Int
  module Int32          = Int32
  module Int64          = Int64
  module Lazy           = Lazy
  (* module Lexing         = Lexing *)
  module List           = List
  module ListLabels     = ListLabels
  module Map            = Map
  (* module Marshal        = Marshal *)
  module MoreLabels     = struct
    module Map = MoreLabels.Map
    module Set = MoreLabels.Set
    (* include MoreLabels.Hashtbl *)
  end
  module _ = (Stdlib.MoreLabels : module type of MoreLabels)  (* see note above *)
  (* module Mutex          = Mutex *)
  module Nativeint      = Nativeint
  (* module Obj            = Obj *)
  (* module Oo             = Oo *)
  module Option         = Option
  (* module Out_channel    = Out_channel *)
  (* module Parsing        = Parsing *)
  module Printexc       = struct
    type t = Printexc.t
    let to_string_default = Printexc.to_string_default
    (* only to_string_default can be expected to be pure, not to_string *)
  end
  module _ = (Stdlib.Printexc : module type of Printexc)  (* see note above *)
  module Printf         = struct
    let sprintf = Printf.sprintf
    let ksprintf = Printf.ksprintf
  end
  module _ = (Stdlib.Printf : module type of Printf)  (* see note above *)
  (* module Queue          = Queue *)
  (* module Random         = Random *)
  module Result         = Result
  module Scanf          = struct
    module Scanning = struct
      type in_channel = Scanf.Scanning.in_channel
      type scanbuf = Scanf.Scanning.in_channel
    end
    type ('a, 'b, 'c, 'd) scanner = ('a, 'b, 'c, 'd) Scanf.scanner
    type ('a, 'b, 'c, 'd) scanner_opt = ('a, 'b, 'c, 'd) Scanf.scanner_opt
    exception Scan_failure = Scanf.Scan_failure
    let sscanf = Scanf.sscanf
    let sscanf_opt = Scanf.sscanf_opt
    let sscanf_format = Scanf.sscanf_format
    let format_from_string = Scanf.format_from_string
    let unescaped = Scanf.unescaped
  end
  module _ = (Stdlib.Scanf : module type of Scanf)  (* see note above *)
  (* module Semaphore      = Semaphore *)
  module Seq            = (Seq : Sig_seq.SafeSeq)
  module Set            = Set
  (* module Stack          = Stack *)
  module StdLabels      = (StdLabels : sig
    module List : module type of StdLabels.List
    module String : module type of (StdLabels.String : Sig_string.SafeStringLabels)
  end)
  module _ = (Stdlib.StdLabels : module type of StdLabels)  (* see note above *)
  module String         = (String : Sig_string.SafeString)
  module StringLabels   = (StringLabels : Sig_string.SafeStringLabels)
  (* module Sys            = Sys *)
  module Uchar          = Uchar
  module Unit           = Unit
  (* module Weak           = Weak *)
end

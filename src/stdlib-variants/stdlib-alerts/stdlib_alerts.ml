(** Variants of {!Stdlib}, but with signature items that can be restricted annotated with alerts.

There is one "variant" of the {!Stdlib} interface implemented, namely {!module-type-Stdlib_alerting}.
Alerts are placed on signature items (types, values, and modules) so that those items can be restricted.

The following alerts are used:

- [physical_eq]: all kinds of physical comparisons
- [debug_macro]: macros like [__LOC__]
- [list_op]: operations on lists
- [input_output]: all kinds of real IO
- [impure]: references, arrays, and other imperative programming constructs
- [unsafe]: generally unsafe operations

By default, for a "safe" sandbox environment, the following alerts should be enabled:
[physical_eq], [input_output], [impure], [unsafe]. The remaining alerts are intended for
specific situtations and exercises where more needs to be restricted.

Similar modules for [Thread] and [Event] are in {!Threads_alerts} to avoid unnecessary dependencies.
*)

[@@@alert "-physical_eq"]
[@@@alert "-debug_macro"]
[@@@alert "-list_op"]
[@@@alert "-input_output"]
[@@@alert "-impure"]
[@@@alert "-unsafe"]

module type Char_alerting = sig
  [%%include stdlib.char (!standard - unsafe_chr)]
end

module type Digest_alerting = sig
  [%%include
    stdlib.digest (t, compare, equal, string, substring, to_hex, from_hex),
    { attributes = __ [@alert impure "Imperative programming is not permitted"];
      items = stdlib.digest (bytes, subbytes) },
    { attributes = __ [@alert input_output "Input/output is not permitted"];
      items = stdlib.digest (channel, file, output, input) }
  ]
end

module type Filename_alerting = sig
  [%%include
    stdlib.filename (!standard - (temp_file, open_temp_file, temp_dir, get_temp_dir_name, set_temp_dir_name)),
    { attributes = __ [@alert input_output "Input/output is not permitted"];
      items = stdlib.filename (temp_file, open_temp_file, temp_dir, get_temp_dir_name, set_temp_dir_name) }
  ]
end

module type Float_alerting = sig
  [%%include
    stdlib.float (!standard - (Array, ArrayLabels)),
    { attributes = __ [@alert impure "Arrays are not permitted"];
      items = stdlib.float (Array, ArrayLabels) }
  ]
end

module type Hashtbl_alerting = sig
  [%%include
    stdlib.hashtbl (hash, seeded_hash, hash_param, seeded_hash_param),
    { attributes = __ [@alert impure "Hash tables are not permitted"];
      items = stdlib.hashtbl (!standard - (hash, seeded_hash, hash_param, seeded_hash_param)) }
  ]
end

module type List_alerting = sig
  [%%include
    stdlib.list (!standard - (memq, assq, assq_opt, mem_assq, remove_assq)),
    { attributes = __ [@alert physical_eq "Physical comparisons are not permitted"];
      items = stdlib.list (memq, assq, assq_opt, mem_assq, remove_assq) }
  ]
end

module type ListLabels_alerting = sig
  [%%include
    stdlib.listLabels (!standard - (memq, assq, assq_opt, mem_assq, remove_assq)),
    { attributes = __ [@alert physical_eq "Physical comparisons are not permitted"];
      items = stdlib.listLabels (memq, assq, assq_opt, mem_assq, remove_assq) }
  ]
end

module type MoreLabels_alerting = sig
  [%%include
    stdlib.moreLabels (Map, Set),
    { attributes = __ [@alert impure "Hash tables are not permitted"];
      items = stdlib.moreLabels (Hashtbl) }
  ]
end

module type Printexc_alerting = sig
  (* only to_string_default can be expected to be pure, not to_string *)
  [%%include
    stdlib.printexc (t, to_string_default),
    { attributes = __ [@alert unsafe "This item is not permitted"];
      items = stdlib.printexc (!standard - (t, to_string_default)) }
  ]
end

module type Printf_alerting = sig
  [%%include
    stdlib.printf (sprintf, ksprintf),
    { attributes = __ [@alert input_output "Input/output is not permitted"];
      items = stdlib.printf (fprintf, printf, eprintf, ifprintf, kfprintf, ikfprintf) },
    { attributes = __ [@alert impure "This imperative programming item is not permitted"];
      items = stdlib.printf (bprintf, ibprintf, kbprintf, ikbprintf) }
  ]
end

module type Scanf_alerting = sig
  module Scanning : sig
    type in_channel
    type scanbuf
  end
  [%%include
    stdlib.scanf (scanner, scanner_opt, Scan_failure, sscanf, sscanf_opt, ksscanf, sscanf_format, format_from_string, unescaped),
    { attributes = __ [@alert input_output "Input/output is not permitted"];
      items = stdlib.scanf (bscanf, bscanf_opt, scanf, scanf_opt, kscanf, bscanf_format) }
  ]
end

module type Seq_alerting = sig
  [%%include
    stdlib.seq (!standard - (once, Forced_twice, to_dispenser)),
    { attributes = __ [@alert impure "This imperative programming item is not permitted"];
      items = stdlib.seq (once, Forced_twice, to_dispenser) }
  ]
end

module type StringLabels_alerting = sig
  [%%include
    stdlib.stringLabels (!standard - (of_bytes, to_bytes, blit, unsafe_get, unsafe_blit)),
    { attributes = __ [@alert impure "This imperative programming item is not permitted"];
      items = stdlib.stringLabels (of_bytes, to_bytes, blit) }
  ]
end

module type StdLabels_alerting = sig
  module String : StringLabels_alerting
  module [@alert list_op "List operations are not permitted"] List : ListLabels_alerting

  [%%include
    { attributes = __ [@alert impure "Arrays are not permitted"];
      items = stdlib.stdLabels (Array) },
    { attributes = __ [@alert impure "This imperative programming module is not permitted"];
      items = stdlib.stdLabels (Bytes) }
  ]
end

module type String_alerting = sig
  [%%include
    stdlib.string (!standard - (of_bytes, to_bytes, blit, unsafe_get, unsafe_blit)),
    { attributes = __ [@alert impure "This imperative programming item is not permitted"];
      items = stdlib.string (of_bytes, to_bytes, blit) }
  ]
end

module type Uchar_alerting = sig
  [%%include stdlib.uchar (!standard - (unsafe_of_int, unsafe_to_char))]
end


module type Stdlib_alerting = sig

  (** Derived from {!Stdlib}, but with items that can be restricted annotated with alerts.

      Things that can be restricted are marked with an {b ⚠️ Alert}.
      Not everything marked here is disabled by default though.
      See {!Stdlib_alerts} for the defaults. *)

  (** {1 [raise] primitives and standard exceptions} *)

  [%%include stdlib.stdlib (
    raise, raise_notrace, failwith, invalid_arg,
    Exit, Match_failure, Assert_failure, Invalid_argument, Failure, Not_found,
    Out_of_memory, Stack_overflow, Sys_error, End_of_file, Division_by_zero,
    Sys_blocked_io, Undefined_recursive_module
  )]

  (** {1 Polymorphic comparisons}
      Not including the impure [(==)] and [(!=)]. *)

  [%%include stdlib.stdlib (( = ), ( <> ), ( < ), ( > ), ( <= ), ( >= ), compare, min, max) ]

  (** {1 Physical comparisons}
      The impure [(==)] and [(!=)] *)

  [%%include
    {
      attributes = __ [@alert physical_eq "Physical comparisons are not permitted. Hint: did you mean = ?"];
      items = stdlib.stdlib ( == )
    },
    {
      attributes = __ [@alert physical_eq "Physical comparisons are not permitted. Hint: did you mean <> ?"];
      items = stdlib.stdlib ( != )
    }
  ]

  (** {1 Boolean operations} *)

  [%%include stdlib.stdlib (not, ( && ), ( || )) ]

  (** {1 Debugging macros}
      These are safe in the sense that referential transparency is preserved,
      if two uses of a macro at separate locations are considered separate
      occurrences. *)

  [%%include {
    attributes = __ [@alert debug_macro "Debugging macros are not permitted"];
    items = stdlib.stdlib (
      __LOC__, __FILE__, __LINE__, __MODULE__, __POS__, __FUNCTION__,
      __LOC_OF__, __LINE_OF__, __POS_OF__
    )
  }]

  (** {1 Composition operators} *)

  [%%include stdlib.stdlib ((|>), (@@)) ]

  (** {1 Integer operations} *)

  [%%include stdlib.stdlib (
    ( ~- ), ( ~+ ), succ, pred, ( + ), ( - ), ( * ), ( / ), ( mod ), abs,
    ( land ), ( lor ), ( lxor ), lnot, ( lsl ), ( lsr ), ( asr ),
    max_int, min_int
  )]

  (** {1 Floating-point operations} *)

  [%%include stdlib.stdlib (
    ( ~-. ), ( ~+. ), ( +. ), ( -. ), ( *. ), ( /. ), ( ** ),
    exp, expm1, acos, asin, atan, atan2, hypot, cos, cosh, acosh,
    log, log10, log1p, sin, sinh, asinh, sqrt, tan, tanh, atanh,
    ceil, floor, abs_float, copysign, mod_float, frexp, ldexp, modf,
    float, float_of_int, truncate, int_of_float, infinity, neg_infinity, nan,
    max_float, min_float, epsilon_float, fpclass, classify_float
  )]

  (** {1 String operations}
      More in {!Stdlib.String} *)

  [%%include stdlib.stdlib ( ^ ) ]

  (** {1 Character operations}
      More in {!Stdlib.Char} *)

  [%%include stdlib.stdlib (int_of_char, char_of_int) ]

  (** {1 Unit operations} *)

  [%%include stdlib.stdlib ignore ]

  (** {1 String conversion functions} *)

  [%%include stdlib.stdlib (
    string_of_bool, bool_of_string_opt, bool_of_string,
    string_of_int, int_of_string_opt, int_of_string,
    string_of_float, float_of_string_opt, float_of_string
  )]

  (** {1 Pair operations} *)

  [%%include stdlib.stdlib (fst, snd) ]

  (** {1 Character operations}
      More in {!Stdlib.List} *)

  [%%include {
    attributes = __ [@alert list_op "List operations are not permitted"];
    items = stdlib.stdlib ( @ )
  }]

  (** {1 I/O operations}
      This is regular, unmocked, IO. *)

  [%%include {
    attributes = __ [@alert input_output "Input/output is not permitted"];
    items = stdlib.stdlib (
      in_channel, out_channel, stdin, stdout, stderr,
      (* Output functions on standard output *)
      print_char, print_string, print_bytes, print_int, print_float,
      print_endline, print_newline,
      (* Output functions on standard error *)
      prerr_char, prerr_string, prerr_bytes, prerr_int, prerr_float,
      prerr_endline, prerr_newline,
      (* Input functions on standard input *)
      read_line, read_int_opt, read_int, read_float_opt, read_float,
      (* General output functions *)
      open_flag, open_out, open_out_bin, open_out_gen, flush, flush_all,
      output_char, output_string, output_bytes, output, output_substring,
      output_byte, output_binary_int, output_value, seek_out, pos_out,
      out_channel_length, close_out, close_out_noerr, set_binary_mode_out,
      (* General input functions *)
      open_in, open_in_bin, open_in_gen,
      input_char, input_line, input, really_input, really_input_string,
      input_byte, input_binary_int, input_value, seek_in, pos_in,
      in_channel_length, close_in, close_in_noerr, set_binary_mode_in,
      (* Operations on large files *)
      LargeFile
    )
  }]

  (** {1 References} *)

  [%%include {
    attributes = __ [@alert impure "References are not permitted"];
    items = stdlib.stdlib (
      ref @ { kind = "type" }, ref @ { kind = "value" },
      ( ! ), ( := ), incr, decr
    )
  }]

  (** {1 Result type}
      More in {!Stdlib.Result} *)

  [%%include stdlib.stdlib result ]

  (** {1 Format strings}
      More in {!Stdlib.Scanf}, {!Stdlib.Printf}, and {!Stdlib.Format} *)

  [%%include stdlib.stdlib (format6, format4, format, format_of_string, ( ^^ )) ]

  (** {1 Program termination} *)

  [%%include {
    attributes = __ [@alert unsafe "This function is not permitted"];
    items = stdlib.stdlib (exit, at_exit)
  }]

  (** {1:modules Standard library modules } *)

  (** {2 Completely safe modules} *)

  [%%include stdlib.stdlib (Bool, Complex, Either, Fun, Int, Int32, Int64, Lazy, Map, Nativeint, Option, Result, Set, Unit)]
  (* TODO: is Lazy *really* safe? *)

  (** {2 Partially safe modules} *)

  module Char : Char_alerting
  module Digest : Digest_alerting
  module Filename : Filename_alerting
  module Float : Float_alerting
  module Hashtbl : Hashtbl_alerting

  module [@alert list_op "List operations are not permitted"] List : List_alerting
  module [@alert list_op "List operations are not permitted"] ListLabels : ListLabels_alerting

  module MoreLabels : MoreLabels_alerting

  module Printexc : Printexc_alerting

  module Printf : Printf_alerting
  module Scanf : Scanf_alerting

  module [@alert list_op "List (and sequence) operations are not permitted"] Seq : Seq_alerting

  module StdLabels : StdLabels_alerting

  module String : String_alerting
  module StringLabels : StringLabels_alerting

  module Uchar : Uchar_alerting

  (** {2 Imperative programming modules} *)

  [%%include {
    attributes = __ [@alert impure "Arrays are not permitted"];
    items = stdlib.stdlib (Array, ArrayLabels, Bigarray)
  }]

  [%%include {
    attributes = __ [@alert impure "This imperative programming module is not permitted"];
    items = stdlib.stdlib (Buffer, Bytes, BytesLabels, Format, Lexing, Parsing, Queue, Random, Stack)
  }]

  [%%include {
    attributes = __ [@alert input_output "Input/output is not permitted"];
    items = stdlib.stdlib (In_channel, Out_channel)
  }]

  (** {2 Unsafe modules}
      Modules containing advanced features for interacting with the runtime system. *)

  [%%include {
    attributes = __ [@alert unsafe "This module is not permitted"];
    items = stdlib.stdlib (Arg, Atomic, Callback, Condition, Domain, Effect, Ephemeron, Gc, Marshal, Mutex, Obj, Oo, Sys, Weak)
  }]

end

(** [Stdlib], but with the above module type. *)
module Stdlib_alerting : Stdlib_alerting = Stdlib

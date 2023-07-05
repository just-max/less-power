(** Adapted from [Stdlib], with different components in different modules. In particular,
    a safe (pure) subset of OCaml features can be enabled by importing a subset
    of modules defined here. *)

module Exceptions = struct
  (** [SAFE] [raise] primitives and standard exceptions *)

  external raise : exn -> 'a = "%raise"
  external raise_notrace : exn -> 'a = "%raise_notrace"

  let failwith s = raise(Failure s)
  let invalid_arg s = raise(Invalid_argument s)

  exception Exit
  exception Match_failure = Match_failure
  exception Assert_failure = Assert_failure
  exception Invalid_argument = Invalid_argument
  exception Failure = Failure
  exception Not_found = Not_found
  exception Out_of_memory = Out_of_memory
  exception Stack_overflow = Stack_overflow
  exception Sys_error = Sys_error
  exception End_of_file = End_of_file
  exception Division_by_zero = Division_by_zero
  exception Sys_blocked_io = Sys_blocked_io
  exception Undefined_recursive_module = Undefined_recursive_module
end

module Composition = struct
  (** [SAFE] Composition operators *)

  external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
  external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
end

module Debugging = struct
  (** [SAFE] Debugging macros *)
  (* These are safe in the sense that referential transparency is preserved, if two uses
      of a macro at separate locations are considered separate occurrences. *)

  external __LOC__ : string = "%loc_LOC"
  external __FILE__ : string = "%loc_FILE"
  external __LINE__ : int = "%loc_LINE"
  external __MODULE__ : string = "%loc_MODULE"
  external __POS__ : string * int * int * int = "%loc_POS"
  external __FUNCTION__ : string = "%loc_FUNCTION"

  external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
  external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
  external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
end

module Comparisons = struct
  (** [SAFE] Polymorphic comparisons. Does not include the impure [(==)] and [(!=)],
      which are in [PhysicalComparisons]. *)

  external ( = ) : 'a -> 'a -> bool = "%equal"
  external ( <> ) : 'a -> 'a -> bool = "%notequal"
  external ( < ) : 'a -> 'a -> bool = "%lessthan"
  external ( > ) : 'a -> 'a -> bool = "%greaterthan"
  external ( <= ) : 'a -> 'a -> bool = "%lessequal"
  external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
  external compare : 'a -> 'a -> int = "%compare"

  let min x y = if x <= y then x else y
  let max x y = if x >= y then x else y
end

module PhysicalComparisons = struct
  (** [UNSAFE] The impure [(==)] and [(!=)] *)

  external ( == ) : 'a -> 'a -> bool = "%eq"
  external ( != ) : 'a -> 'a -> bool = "%noteq"
end

module BooleanOperations = struct
  (** [SAFE] Boolean operations *)

  external not : bool -> bool = "%boolnot"
  external ( && ) : bool -> bool -> bool = "%sequand"
  external ( || ) : bool -> bool -> bool = "%sequor"
end

module IntegerOperations = struct
  (** [SAFE] Integer operations *)

  open Comparisons

  external ( ~- ) : int -> int = "%negint"
  external ( ~+ ) : int -> int = "%identity"
  external succ : int -> int = "%succint"
  external pred : int -> int = "%predint"
  external ( + ) : int -> int -> int = "%addint"
  external ( - ) : int -> int -> int = "%subint"
  external ( * ) : int -> int -> int = "%mulint"
  external ( / ) : int -> int -> int = "%divint"
  external ( mod ) : int -> int -> int = "%modint"

  let abs x = if x >= 0 then x else -x

  external ( land ) : int -> int -> int = "%andint"
  external ( lor ) : int -> int -> int = "%orint"
  external ( lxor ) : int -> int -> int = "%xorint"

  let lnot x = x lxor (-1)

  external ( lsl ) : int -> int -> int = "%lslint"
  external ( lsr ) : int -> int -> int = "%lsrint"
  external ( asr ) : int -> int -> int = "%asrint"

  let max_int = (-1) lsr 1
  let min_int = max_int + 1
end

module FloatingPointOperations = struct
  (** [SAFE] Floating-point operations *)

  external ( ~-. ) : float -> float = "%negfloat"
  external ( ~+. ) : float -> float = "%identity"
  external ( +. ) : float -> float -> float = "%addfloat"
  external ( -. ) : float -> float -> float = "%subfloat"
  external ( *. ) : float -> float -> float = "%mulfloat"
  external ( /. ) : float -> float -> float = "%divfloat"
  external ( ** ) : float -> float -> float = "caml_power_float" "pow"
    [@@unboxed] [@@noalloc]
  external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
  external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
    [@@unboxed] [@@noalloc]
  external acos : float -> float = "caml_acos_float" "acos"
    [@@unboxed] [@@noalloc]
  external asin : float -> float = "caml_asin_float" "asin"
    [@@unboxed] [@@noalloc]
  external atan : float -> float = "caml_atan_float" "atan"
    [@@unboxed] [@@noalloc]
  external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
    [@@unboxed] [@@noalloc]
  external hypot : float -> float -> float
                = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
  external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
  external cosh : float -> float = "caml_cosh_float" "cosh"
    [@@unboxed] [@@noalloc]
  external acosh : float -> float = "caml_acosh_float" "caml_acosh"
    [@@unboxed] [@@noalloc]
  external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
  external log10 : float -> float = "caml_log10_float" "log10"
    [@@unboxed] [@@noalloc]
  external log1p : float -> float = "caml_log1p_float" "caml_log1p"
    [@@unboxed] [@@noalloc]
  external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
  external sinh : float -> float = "caml_sinh_float" "sinh"
    [@@unboxed] [@@noalloc]
  external asinh : float -> float = "caml_asinh_float" "caml_asinh"
    [@@unboxed] [@@noalloc]
  external sqrt : float -> float = "caml_sqrt_float" "sqrt"
    [@@unboxed] [@@noalloc]
  external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
  external tanh : float -> float = "caml_tanh_float" "tanh"
    [@@unboxed] [@@noalloc]
  external atanh : float -> float = "caml_atanh_float" "caml_atanh"
    [@@unboxed] [@@noalloc]
  external ceil : float -> float = "caml_ceil_float" "ceil"
    [@@unboxed] [@@noalloc]
  external floor : float -> float = "caml_floor_float" "floor"
    [@@unboxed] [@@noalloc]
  external abs_float : float -> float = "%absfloat"
  external copysign : float -> float -> float
                    = "caml_copysign_float" "caml_copysign"
                    [@@unboxed] [@@noalloc]
  external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
    [@@unboxed] [@@noalloc]
  external frexp : float -> float * int = "caml_frexp_float"
  external ldexp : (float [@unboxed]) -> (int [@untagged]) -> (float [@unboxed]) =
    "caml_ldexp_float" "caml_ldexp_float_unboxed" [@@noalloc]
  external modf : float -> float * float = "caml_modf_float"
  external float : int -> float = "%floatofint"
  external float_of_int : int -> float = "%floatofint"
  external truncate : float -> int = "%intoffloat"
  external int_of_float : float -> int = "%intoffloat"
  (* external float_of_bits : int64 -> float
    = "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
    [@@unboxed] [@@noalloc] *)
  let infinity = Stdlib.infinity
  let neg_infinity = Stdlib.neg_infinity
  let nan = Stdlib.nan
  let max_float = Stdlib.max_float
  let min_float = Stdlib.min_float
  let epsilon_float = Stdlib.epsilon_float

  type fpclass = Stdlib.fpclass
  external classify_float : (float [@unboxed]) -> fpclass =
    "caml_classify_float" "caml_classify_float_unboxed" [@@noalloc]
end

module StringOperations = struct
  (** [SAFE] String operations -- more in Stdlib module String *)

  let ( ^ ) = Stdlib.( ^ )
end

module CharOperations = struct
  (** [SAFE] Character operations -- more in module Char *)

  external int_of_char : char -> int = "%identity"
  let char_of_int = Stdlib.char_of_int
end

module UnitOperations = struct
  (** [SAFE] Unit operations *)

  external ignore : 'a -> unit = "%ignore"
end

module PairOperations = struct
  (** [SAFE] Pair operations *)

  external fst : 'a * 'b -> 'a = "%field0"
  external snd : 'a * 'b -> 'b = "%field1"
end

module References = struct
  (** [UNSAFE] References *)

  type 'a ref = 'a Stdlib.ref
  external ref : 'a -> 'a ref = "%makemutable"
  external ( ! ) : 'a ref -> 'a = "%field0"
  external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
  external incr : int ref -> unit = "%incr"
  external decr : int ref -> unit = "%decr"
end

module Result = struct
  (** [SAFE] Result type *)

  type ('a,'b) result = ('a,'b) Stdlib.result
end

module StringConversion = struct
  (** [SAFE] String conversion functions *)

  let string_of_bool = Stdlib.string_of_bool
  let bool_of_string = Stdlib.bool_of_string

  let bool_of_string_opt = Stdlib.bool_of_string_opt

  let string_of_int = Stdlib.string_of_int

  external int_of_string : string -> int = "caml_int_of_string"

  let int_of_string_opt = Stdlib.int_of_string_opt

  let valid_float_lexem = Stdlib.valid_float_lexem

  let string_of_float = Stdlib.string_of_float

  external float_of_string : string -> float = "caml_float_of_string"

  let float_of_string_opt = Stdlib.float_of_string_opt
end

module ListOperations = struct
  (** [SAFE] List operations -- more in module List *)

  let ( @ ) = Stdlib.( @ )
end

module IOOperations = struct
  (** [UNSAFE] I/O operations. This is regular, unmocked, IO. *)

  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel

  let stdin = Stdlib.stdin
  let stdout = Stdlib.stdout
  let stderr = Stdlib.stderr

  (* General output functions *)

  type open_flag = Stdlib.open_flag

  let open_out_gen = Stdlib.open_out_gen

  let open_out = Stdlib.open_out

  let open_out_bin = Stdlib.open_out_bin

  let flush = Stdlib.flush

  let flush_all = Stdlib.flush_all

  let output_char = Stdlib.output_char

  let output_bytes = Stdlib.output_bytes

  let output_string = Stdlib.output_string

  let output = Stdlib.output

  let output_substring = Stdlib.output_substring

  let output_byte = Stdlib.output_byte
  let output_binary_int = Stdlib.output_binary_int

  let output_value = Stdlib.output_value

  let seek_out = Stdlib.seek_out
  let pos_out = Stdlib.pos_out
  let out_channel_length = Stdlib.out_channel_length
  let close_out = Stdlib.close_out

  let close_out_noerr = Stdlib.close_out_noerr

  (* General input functions *)

  let open_in_gen = Stdlib.open_in_gen

  let open_in = Stdlib.open_in

  let open_in_bin = Stdlib.open_in_bin

  let input_char = Stdlib.input_char

  let input = Stdlib.input

  let unsafe_really_input = Stdlib.unsafe_really_input

  let really_input = Stdlib.really_input

  let really_input_string = Stdlib.really_input_string

  let input_line = Stdlib.input_line

  let input_byte = Stdlib.input_byte
  let input_binary_int = Stdlib.input_binary_int
  let input_value = Stdlib.input_value
  let seek_in = Stdlib.seek_in
  let pos_in = Stdlib.pos_in
  let in_channel_length = Stdlib.in_channel_length
  let close_in = Stdlib.close_in
  let close_in_noerr = Stdlib.close_in_noerr

  let set_binary_mode_in = Stdlib.set_binary_mode_in


  (* Output functions on standard output *)

  let print_char = Stdlib.print_char
  let print_string = Stdlib.print_string
  let print_bytes = Stdlib.print_bytes
  let print_int = Stdlib.print_int
  let print_float = Stdlib.print_float
  let print_endline = Stdlib.print_endline
  let print_newline = Stdlib.print_newline


  (* Output functions on standard error *)

  let prerr_char = Stdlib.prerr_char
  let prerr_string = Stdlib.prerr_string
  let prerr_bytes = Stdlib.prerr_bytes
  let prerr_int = Stdlib.prerr_int
  let prerr_float = Stdlib.prerr_float
  let prerr_endline = Stdlib.prerr_endline
  let prerr_newline = Stdlib.prerr_newline


  (* Input functions on standard input *)

  let read_line = Stdlib.read_line
  let read_int = Stdlib.read_int
  let read_int_opt = Stdlib.read_int_opt
  let read_float = Stdlib.read_float
  let read_float_opt = Stdlib.read_float_opt


  (* Operations on large files *)

  module LargeFile =
    struct
      external seek_out : out_channel -> int64 -> unit = "caml_ml_seek_out_64"
      external pos_out : out_channel -> int64 = "caml_ml_pos_out_64"
      external out_channel_length : out_channel -> int64
                                  = "caml_ml_channel_size_64"
      external seek_in : in_channel -> int64 -> unit = "caml_ml_seek_in_64"
      external pos_in : in_channel -> int64 = "caml_ml_pos_in_64"
      external in_channel_length : in_channel -> int64 = "caml_ml_channel_size_64"
    end
end

module Formats = struct
  (** [SAFE] Formats *)

  type ('a, 'b, 'c, 'd, 'e, 'f) format6
    = ('a, 'b, 'c, 'd, 'e, 'f) Stdlib.format6

  type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) Stdlib.format4

  type ('a, 'b, 'c) format = ('a, 'b, 'c) Stdlib.format

  let string_of_format = Stdlib.string_of_format

  external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"

  let ( ^^ ) = Stdlib.( ^^ )
end

module Exit = struct
  (** [UNSAFE] Functions exiting the program [at_exit], [exit] *)
  (* Unsafe since the program can be terminated at any time,
      and at_exit provides a sort of state. *)

  let at_exit = Stdlib.at_exit
  let do_domain_local_at_exit = Stdlib.do_domain_local_at_exit
  let do_at_exit = Stdlib.do_at_exit
  let exit = Stdlib.exit
end

module SafeAliases = Stdlib_aliases.SafeAliases

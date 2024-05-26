module type SafeFloat = sig
  (** As module type of [Float], but without [Array] and [ArrayLabels]. *)

  val zero : float
  val one : float
  val minus_one : float
  external neg : float -> float = "%negfloat"
  external add : float -> float -> float = "%addfloat"
  external sub : float -> float -> float = "%subfloat"
  external mul : float -> float -> float = "%mulfloat"
  external div : float -> float -> float = "%divfloat"

  external fma : float -> float -> float -> float
    = "caml_fma_float" "caml_fma"
    [@@unboxed] [@@noalloc]

  external rem : float -> float -> float
    = "caml_fmod_float" "fmod"
    [@@unboxed] [@@noalloc]

  val succ : float -> float
  val pred : float -> float
  external abs : float -> float = "%absfloat"
  val infinity : float
  val neg_infinity : float
  val nan : float
  val signaling_nan : float
  val quiet_nan : float
  val pi : float
  val max_float : float
  val min_float : float
  val epsilon : float
  val is_finite : float -> bool
  val is_infinite : float -> bool
  val is_nan : float -> bool
  val is_integer : float -> bool
  external of_int : int -> float = "%floatofint"
  external to_int : float -> int = "%intoffloat"
  external of_string : string -> float = "caml_float_of_string"
  val of_string_opt : string -> float option
  val to_string : float -> string

  type fpclass = Stdlib.Float.fpclass (* = ...? *)

  external classify_float : (float[@unboxed]) -> fpclass
    = "caml_classify_float" "caml_classify_float_unboxed"
    [@@noalloc]

  external pow : float -> float -> float
    = "caml_power_float" "pow"
    [@@unboxed] [@@noalloc]

  external sqrt : float -> float = "caml_sqrt_float" "sqrt"
    [@@unboxed] [@@noalloc]

  external cbrt : float -> float
    = "caml_cbrt_float" "caml_cbrt"
    [@@unboxed] [@@noalloc]

  external exp : float -> float = "caml_exp_float" "exp"
    [@@unboxed] [@@noalloc]

  external exp2 : float -> float
    = "caml_exp2_float" "caml_exp2"
    [@@unboxed] [@@noalloc]

  external log : float -> float = "caml_log_float" "log"
    [@@unboxed] [@@noalloc]

  external log10 : float -> float = "caml_log10_float" "log10"
    [@@unboxed] [@@noalloc]

  external log2 : float -> float
    = "caml_log2_float" "caml_log2"
    [@@unboxed] [@@noalloc]

  external expm1 : float -> float
    = "caml_expm1_float" "caml_expm1"
    [@@unboxed] [@@noalloc]

  external log1p : float -> float
    = "caml_log1p_float" "caml_log1p"
    [@@unboxed] [@@noalloc]

  external cos : float -> float = "caml_cos_float" "cos"
    [@@unboxed] [@@noalloc]

  external sin : float -> float = "caml_sin_float" "sin"
    [@@unboxed] [@@noalloc]

  external tan : float -> float = "caml_tan_float" "tan"
    [@@unboxed] [@@noalloc]

  external acos : float -> float = "caml_acos_float" "acos"
    [@@unboxed] [@@noalloc]

  external asin : float -> float = "caml_asin_float" "asin"
    [@@unboxed] [@@noalloc]

  external atan : float -> float = "caml_atan_float" "atan"
    [@@unboxed] [@@noalloc]

  external atan2 : float -> float -> float
    = "caml_atan2_float" "atan2"
    [@@unboxed] [@@noalloc]

  external hypot : float -> float -> float
    = "caml_hypot_float" "caml_hypot"
    [@@unboxed] [@@noalloc]

  external cosh : float -> float = "caml_cosh_float" "cosh"
    [@@unboxed] [@@noalloc]

  external sinh : float -> float = "caml_sinh_float" "sinh"
    [@@unboxed] [@@noalloc]

  external tanh : float -> float = "caml_tanh_float" "tanh"
    [@@unboxed] [@@noalloc]

  external acosh : float -> float
    = "caml_acosh_float" "caml_acosh"
    [@@unboxed] [@@noalloc]

  external asinh : float -> float
    = "caml_asinh_float" "caml_asinh"
    [@@unboxed] [@@noalloc]

  external atanh : float -> float
    = "caml_atanh_float" "caml_atanh"
    [@@unboxed] [@@noalloc]

  external erf : float -> float = "caml_erf_float" "caml_erf"
    [@@unboxed] [@@noalloc]

  external erfc : float -> float
    = "caml_erfc_float" "caml_erfc"
    [@@unboxed] [@@noalloc]

  external trunc : float -> float
    = "caml_trunc_float" "caml_trunc"
    [@@unboxed] [@@noalloc]

  external round : float -> float
    = "caml_round_float" "caml_round"
    [@@unboxed] [@@noalloc]

  external ceil : float -> float = "caml_ceil_float" "ceil"
    [@@unboxed] [@@noalloc]

  external floor : float -> float = "caml_floor_float" "floor"
    [@@unboxed] [@@noalloc]

  external next_after : float -> float -> float
    = "caml_nextafter_float" "caml_nextafter"
    [@@unboxed] [@@noalloc]

  external copy_sign : float -> float -> float
    = "caml_copysign_float" "caml_copysign"
    [@@unboxed] [@@noalloc]

  external sign_bit : (float[@unboxed]) -> bool
    = "caml_signbit_float" "caml_signbit"
    [@@noalloc]

  external frexp : float -> float * int = "caml_frexp_float"

  external ldexp :
    (float[@unboxed]) -> (int[@untagged]) -> (float[@unboxed])
    = "caml_ldexp_float" "caml_ldexp_float_unboxed"
    [@@noalloc]

  external modf : float -> float * float = "caml_modf_float"

  type t = float

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val min : t -> t -> t
  val max : float -> float -> float
  val min_max : float -> float -> float * float
  val min_num : t -> t -> t
  val max_num : t -> t -> t
  val min_max_num : float -> float -> float * float
  val seeded_hash : int -> t -> int
  val hash : t -> int

end

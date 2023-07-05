module type SafeString = sig
  (** As module type of [String], but without some unsafe buffer-related functions. *)

  type t = Stdlib.String.t

  val make : int -> char -> string
  val init : int -> (int -> char) -> string
  val empty : string
  external length : string -> int = "%string_length"
  external get : string -> int -> char = "%string_safe_get"
  val concat : string -> string list -> string
  val cat : string -> string -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val starts_with : prefix:string -> string -> bool
  val ends_with : suffix:string -> string -> bool
  val contains_from : string -> int -> char -> bool
  val rcontains_from : string -> int -> char -> bool
  val contains : string -> char -> bool
  val sub : string -> int -> int -> string
  val split_on_char : char -> string -> string list
  val map : (char -> char) -> string -> string
  val mapi : (int -> char -> char) -> string -> string
  val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
  val for_all : (char -> bool) -> string -> bool
  val exists : (char -> bool) -> string -> bool
  val trim : string -> string
  val escaped : string -> string
  val uppercase_ascii : string -> string
  val lowercase_ascii : string -> string
  val capitalize_ascii : string -> string
  val uncapitalize_ascii : string -> string
  val iter : (char -> unit) -> string -> unit
  val iteri : (int -> char -> unit) -> string -> unit
  val index_from : string -> int -> char -> int
  val index_from_opt : string -> int -> char -> int option
  val rindex_from : string -> int -> char -> int
  val rindex_from_opt : string -> int -> char -> int option
  val index : string -> char -> int
  val index_opt : string -> char -> int option
  val rindex : string -> char -> int
  val rindex_opt : string -> char -> int option
  val to_seq : t -> char Stdlib.Seq.t
  val to_seqi : t -> (int * char) Stdlib.Seq.t
  val of_seq : char Stdlib.Seq.t -> t
  val get_utf_8_uchar : t -> int -> Stdlib.Uchar.utf_decode
  val is_valid_utf_8 : t -> bool
  val get_utf_16be_uchar : t -> int -> Stdlib.Uchar.utf_decode
  val is_valid_utf_16be : t -> bool
  val get_utf_16le_uchar : t -> int -> Stdlib.Uchar.utf_decode
  val is_valid_utf_16le : t -> bool
  val get_uint8 : string -> int -> int
  val get_int8 : string -> int -> int
  val get_uint16_ne : string -> int -> int
  val get_uint16_be : string -> int -> int
  val get_uint16_le : string -> int -> int
  val get_int16_ne : string -> int -> int
  val get_int16_be : string -> int -> int
  val get_int16_le : string -> int -> int
  val get_int32_ne : string -> int -> int32
  val hash : t -> int
  val seeded_hash : int -> t -> int
  val get_int32_be : string -> int -> int32
  val get_int32_le : string -> int -> int32
  val get_int64_ne : string -> int -> int64
  val get_int64_be : string -> int -> int64
  val get_int64_le : string -> int -> int64
end

module type SafeStringLabels = sig
  type t = Stdlib.String.t

  val make : int -> char -> string
  val init : int -> f:(int -> char) -> string
  val empty : string
  external length : string -> int = "%string_length"
  external get : string -> int -> char = "%string_safe_get"
  val concat : sep:string -> string list -> string
  val cat : string -> string -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val starts_with : prefix:string -> string -> bool
  val ends_with : suffix:string -> string -> bool
  val contains_from : string -> int -> char -> bool
  val rcontains_from : string -> int -> char -> bool
  val contains : string -> char -> bool
  val sub : string -> pos:int -> len:int -> string
  val split_on_char : sep:char -> string -> string list
  val map : f:(char -> char) -> string -> string
  val mapi : f:(int -> char -> char) -> string -> string

  val fold_left :
    f:('a -> char -> 'a) -> init:'a -> string -> 'a

  val fold_right :
    f:(char -> 'a -> 'a) -> string -> init:'a -> 'a

  val for_all : f:(char -> bool) -> string -> bool
  val exists : f:(char -> bool) -> string -> bool
  val trim : string -> string
  val escaped : string -> string
  val uppercase_ascii : string -> string
  val lowercase_ascii : string -> string
  val capitalize_ascii : string -> string
  val uncapitalize_ascii : string -> string
  val iter : f:(char -> unit) -> string -> unit
  val iteri : f:(int -> char -> unit) -> string -> unit
  val index_from : string -> int -> char -> int
  val index_from_opt : string -> int -> char -> int option
  val rindex_from : string -> int -> char -> int
  val rindex_from_opt : string -> int -> char -> int option
  val index : string -> char -> int
  val index_opt : string -> char -> int option
  val rindex : string -> char -> int
  val rindex_opt : string -> char -> int option
  val to_seq : t -> char Stdlib.Seq.t
  val to_seqi : t -> (int * char) Stdlib.Seq.t
  val of_seq : char Stdlib.Seq.t -> t
  val get_utf_8_uchar : t -> int -> Stdlib.Uchar.utf_decode
  val is_valid_utf_8 : t -> bool
  val get_utf_16be_uchar : t -> int -> Stdlib.Uchar.utf_decode
  val is_valid_utf_16be : t -> bool
  val get_utf_16le_uchar : t -> int -> Stdlib.Uchar.utf_decode
  val is_valid_utf_16le : t -> bool

  val get_uint8 : string -> int -> int
  val get_int8 : string -> int -> int
  val get_uint16_ne : string -> int -> int
  val get_uint16_be : string -> int -> int
  val get_uint16_le : string -> int -> int
  val get_int16_ne : string -> int -> int
  val get_int16_be : string -> int -> int
  val get_int16_le : string -> int -> int
  val get_int32_ne : string -> int -> int32
  val hash : t -> int
  val seeded_hash : int -> t -> int
  val get_int32_be : string -> int -> int32
  val get_int32_le : string -> int -> int32
  val get_int64_ne : string -> int -> int64
  val get_int64_be : string -> int -> int64
  val get_int64_le : string -> int -> int64
end

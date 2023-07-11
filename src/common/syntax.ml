(** [let*] and [let+] syntax *)

module type Monad1 = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module MkSyntaxMonad1 (X : Monad1) = struct
  let ( let+ ) m f = X.map f m
  let ( let* ) = X.bind

  let ( and+ ) m1 m2 =
    let* x1 = m1 in
    let+ x2 = m2 in
    (x1, x2)

  let ( and* ) = ( and+ )
end

module type Monad2 = sig
  type ('a, 'b) t

  val map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
  val bind : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t
  val return : 'a -> ('a, 'b) t
end

module MkSyntaxMonad2 (X : Monad2) = struct
  let ( let+ ) m f = X.map f m
  let ( let* ) = X.bind

  let ( and+ ) m1 m2 =
    let* x1 = m1 in
    let+ x2 = m2 in
    (x1, x2)

  let ( and* ) = ( and+ )
end

module Option =
  MkSyntaxMonad1 (struct  include Option  let return = some  end)
module Result =
  MkSyntaxMonad2 (struct  include Result  let return = ok  end)

module Seq =
  MkSyntaxMonad1 (struct  include Seq  let bind x f = Seq.flat_map f x  end)
module List =
  MkSyntaxMonad1 (struct  include List let bind x f = concat_map f x  let return x = [x]  end)

module Error_context = struct
  module Many = MkSyntaxMonad2 (Error_context.Many)
end

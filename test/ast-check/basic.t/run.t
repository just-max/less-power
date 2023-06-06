Check that we cover the basics, as promised in the documentation of Ast_check

  $ lp-ast-check class.ml
  File "class.ml", lines 2-17, characters 0-3:
   2 | class ['a] queue = object(self)
   3 |   val mutable values : 'a list * 'a list = [], []
   4 | 
   5 |   method pop = match values with
   6 |     | [], [] -> None
  ...
  14 |   method push y =
  15 |     let xs, ys = values in
  16 |     values <- xs, y :: ys
  17 | end
  Error: This is a use of classes, which is not permitted
  File "class.ml", line 8, characters 6-22:
  8 |       values <- xs, ys ;
            ^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  File "class.ml", line 11, characters 6-31:
  11 |       values <- List.rev ys, [] ;
             ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  File "class.ml", line 12, characters 6-14:
  12 |       self#pop
             ^^^^^^^^
  Error: This is a use of classes, which is not permitted
  File "class.ml", line 16, characters 4-25:
  16 |     values <- xs, y :: ys
           ^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  File "class.ml", line 22, characters 61-70:
  22 |   let my_queue : < pop : int option ; push : int -> unit > = new queue in
                                                                    ^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  File "class.ml", line 23, characters 12-25:
  23 |   List.iter my_queue#push [1; 2; 3] ;
                   ^^^^^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  File "class.ml", line 24, characters 18-45:
  24 |   let dispenser = (my_queue :> int dispenser) in
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  File "class.ml", line 25, characters 2-15:
  25 |   dispenser#pop
         ^^^^^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  [1]

  $ lp-ast-check loop.ml
  File "loop.ml", lines 3-5, characters 2-6:
  3 | ..while true do
  4 |     my_fork_bomb (); my_fork_bomb ()
  5 |   done
  Error: This is a use of a while-loop, which is not permitted
  File "loop.ml", lines 9-11, characters 2-6:
   9 | ..for i = 2 to n do
  10 |     if n mod i = 0 then is_prime := true
  11 |   done..
  Error: This is a use of a for-loop, which is not permitted
  [1]

  $ lp-ast-check mut_record.ml
  File "mut_record.ml", line 2, characters 17-38:
  2 | type 'a cell = { mutable contents : 'a }
                       ^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  File "mut_record.ml", line 6, characters 34-70:
  6 |   List.iter (fun x -> if p x then count.contents <- count.contents + 1) xs ;
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  [1]

  $ lp-ast-check mut_record.ml
  File "mut_record.ml", line 2, characters 17-38:
  2 | type 'a cell = { mutable contents : 'a }
                       ^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  File "mut_record.ml", line 6, characters 34-70:
  6 |   List.iter (fun x -> if p x then count.contents <- count.contents + 1) xs ;
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  [1]

  $ lp-ast-check array.ml
  File "array.ml", line 3, characters 13-24:
  3 |   let arrr = [|1; 2; 3|] in
                   ^^^^^^^^^^^
  Error: This is a use of array syntax, which is not permitted
  [1]

  $ lp-ast-check external.ml
  File "external.ml", lines 2-3, characters 0-35:
  2 | external super_secret_caml_function : float -> float
  3 |   = "caml_asinh_float" "caml_asinh"
  Error: This is a use of an external definition, which is not permitted
  [1]

  $ lp-ast-check internal_mod.ml
  File "internal_mod.ml", line 2, characters 7-11:
  2 | module A__B = struct
             ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 6, characters 12-16:
  6 | type 'a s = C__D of 'a | D__F
                  ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 6, characters 25-29:
  6 | type 'a s = C__D of 'a | D__F
                               ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 8, characters 19-23:
  8 | let this__is__ok = C__D "<-- but this is not"
                         ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 10, characters 12-26:
  10 | module type Cant__Do__This = sig val this__is__ok__too : int end
                   ^^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 12, characters 8-14:
  12 | let x : A__B.t s = failwith "oh no"
               ^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 12, characters 8-14:
  12 | let x : A__B.t s = failwith "oh no"
               ^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 16, characters 12-25:
  16 |     include Stdlib__Array
                   ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 20, characters 9-22:
  20 |     open Stdlib__Array
                ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 24, characters 18-31:
  24 |   module M4 = M3 (Stdlib__Array)
                         ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 26, characters 32-45:
  26 |   module M5 (A : module type of Stdlib__Array) = struct end
                                       ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 27, characters 18-31:
  27 |   module M6 = M5 (Stdlib__Array)
                         ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 31, characters 13-26:
  31 |     let open Stdlib__Array in
                    ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  File "internal_mod.ml", line 34, characters 10-30:
  34 |   let y = Stdlib__Array.append
                 ^^^^^^^^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  [1]

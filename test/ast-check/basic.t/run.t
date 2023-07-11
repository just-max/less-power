Check that we cover the basics, as promised in the documentation of Ast_check

  $ lp-ast-check class.ml
  File "class.ml", lines 1-16, characters 0-3:
   1 | class ['a] queue = object(self)
   2 |   val mutable values : 'a list * 'a list = [], []
   3 | 
   4 |   method pop = match values with
   5 |     | [], [] -> None
  ...
  13 |   method push y =
  14 |     let xs, ys = values in
  15 |     values <- xs, y :: ys
  16 | end
  Error: This is a use of classes, which is not permitted
  
  File "class.ml", line 7, characters 6-22:
  7 |       values <- xs, ys ;
            ^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  
  File "class.ml", line 10, characters 6-31:
  10 |       values <- List.rev ys, [] ;
             ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  
  File "class.ml", line 11, characters 6-14:
  11 |       self#pop
             ^^^^^^^^
  Error: This is a use of classes, which is not permitted
  
  File "class.ml", line 15, characters 4-25:
  15 |     values <- xs, y :: ys
           ^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  
  File "class.ml", line 21, characters 61-70:
  21 |   let my_queue : < pop : int option ; push : int -> unit > = new queue in
                                                                    ^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  
  File "class.ml", line 22, characters 12-25:
  22 |   List.iter my_queue#push [1; 2; 3] ;
                   ^^^^^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  
  File "class.ml", line 23, characters 18-45:
  23 |   let dispenser = (my_queue :> int dispenser) in
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  
  File "class.ml", line 24, characters 2-15:
  24 |   dispenser#pop
         ^^^^^^^^^^^^^
  Error: This is a use of classes, which is not permitted
  [1]

  $ lp-ast-check loop.ml
  File "loop.ml", lines 2-4, characters 2-6:
  2 | ..while true do
  3 |     my_fork_bomb (); my_fork_bomb ()
  4 |   done
  Error: This is a use of a while-loop, which is not permitted
  
  File "loop.ml", lines 8-10, characters 2-6:
   8 | ..for i = 2 to n do
   9 |     if n mod i = 0 then is_prime := true
  10 |   done..
  Error: This is a use of a for-loop, which is not permitted
  [1]

  $ lp-ast-check mut_record.ml
  File "mut_record.ml", line 1, characters 17-38:
  1 | type 'a cell = { mutable contents : 'a }
                       ^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  
  File "mut_record.ml", line 5, characters 34-70:
  5 |   List.iter (fun x -> if p x then count.contents <- count.contents + 1) xs ;
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  [1]

  $ lp-ast-check mut_record.ml
  File "mut_record.ml", line 1, characters 17-38:
  1 | type 'a cell = { mutable contents : 'a }
                       ^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  
  File "mut_record.ml", line 5, characters 34-70:
  5 |   List.iter (fun x -> if p x then count.contents <- count.contents + 1) xs ;
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This is a use of a mutable field, which is not permitted
  [1]

  $ lp-ast-check array.ml
  File "array.ml", line 2, characters 13-24:
  2 |   let arrr = [|1; 2; 3|] in
                   ^^^^^^^^^^^
  Error: This is a use of array syntax, which is not permitted
  [1]

  $ lp-ast-check external.ml
  File "external.ml", lines 1-2, characters 0-35:
  1 | external super_secret_caml_function : float -> float
  2 |   = "caml_asinh_float" "caml_asinh"
  Error: This is a use of an external definition, which is not permitted
  [1]

  $ lp-ast-check internal_mod.ml
  File "internal_mod.ml", line 1, characters 7-11:
  1 | module A__B = struct
             ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 5, characters 12-16:
  5 | type 'a s = C__D of 'a | D__F
                  ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 5, characters 25-29:
  5 | type 'a s = C__D of 'a | D__F
                               ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 7, characters 19-23:
  7 | let this__is__ok = C__D "<-- but this is not"
                         ^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 9, characters 12-26:
  9 | module type Cant__Do__This = sig val this__is__ok__too : int end
                  ^^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 11, characters 8-14:
  11 | let x : A__B.t s = failwith "oh no"
               ^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 11, characters 8-14:
  11 | let x : A__B.t s = failwith "oh no"
               ^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 15, characters 12-25:
  15 |     include Stdlib__Array
                   ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 19, characters 9-22:
  19 |     open Stdlib__Array
                ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 23, characters 18-31:
  23 |   module M4 = M3 (Stdlib__Array)
                         ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 25, characters 32-45:
  25 |   module M5 (A : module type of Stdlib__Array) = struct end
                                       ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 26, characters 18-31:
  26 |   module M6 = M5 (Stdlib__Array)
                         ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 30, characters 13-26:
  30 |     let open Stdlib__Array in
                    ^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  
  File "internal_mod.ml", line 33, characters 10-30:
  33 |   let y = Stdlib__Array.append
                 ^^^^^^^^^^^^^^^^^^^^
  Error: This identifier contains a name that starts with an Uppercase letter and contains Two__Underscores in a row
  The use of identifiers of this form is not permitted
  [1]

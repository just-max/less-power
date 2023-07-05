class ['a] queue = object(self)
  val mutable values : 'a list * 'a list = [], []

  method pop = match values with
    | [], [] -> None
    | x :: xs, ys ->
      values <- xs, ys ;
      Some x
    | [], ys ->
      values <- List.rev ys, [] ;
      self#pop

  method push y =
    let xs, ys = values in
    values <- xs, y :: ys
end

type 'a dispenser = < pop : 'a option >

let main () =
  let my_queue : < pop : int option ; push : int -> unit > = new queue in
  List.iter my_queue#push [1; 2; 3] ;
  let dispenser = (my_queue :> int dispenser) in
  dispenser#pop

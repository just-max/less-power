type 'a cell = { mutable contents : 'a }

let mutable_count p xs =
  let count = { contents = 0 } in
  List.iter (fun x -> if p x then count.contents <- count.contents + 1) xs ;
  count.contents

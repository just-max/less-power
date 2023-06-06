
let main () =
  let arrr = [|1; 2; 3|] in
  (* TODO: array assignments are not checked *)
  let pirate arr = arr.(0) <- arr.(1) + arr.(2) in
  pirate arrr ;
  arrr.(0)

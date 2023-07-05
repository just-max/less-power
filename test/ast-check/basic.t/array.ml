let main () =
  let arrr = [|1; 2; 3|] in
  (* array assignments are silently translated to calls to Array.set,
     and thus not checked here *)
  let pirate arr = arr.(0) <- arr.(1) + arr.(2) in
  pirate arrr ;
  arrr.(0)

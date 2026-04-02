let rec my_fork_bomb () =
  while true do
    my_fork_bomb (); my_fork_bomb ()
  done

let my_prime_test n =
  let is_prime = ref true in
  for i = 2 to n do
    if n mod i = 0 then is_prime := true
  done ;
  !is_prime

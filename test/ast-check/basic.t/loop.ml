let rec my_fork_bomb () =
  while true do
    my_fork_bomb (); my_fork_bomb ()
  done

let count_up_to n =
  for i = 2 to n do
    ()
  done

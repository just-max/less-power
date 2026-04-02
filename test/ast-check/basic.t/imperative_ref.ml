let r = ref 0
let x = !r
let () = r := 1
let s = ref 0
let () = incr s
let () = decr s
let _ = (ref : int -> int ref) 0
let _ = List.map ref []

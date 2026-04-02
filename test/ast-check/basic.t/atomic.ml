type r = { mutable n : int [@atomic] }
let _ = Atomic.make 0
let _ = (Atomic.make : int -> int Atomic.t) 0

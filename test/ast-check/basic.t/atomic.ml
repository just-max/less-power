type r = { mutable n : int [@atomic] }
let _ = Atomic.make 0

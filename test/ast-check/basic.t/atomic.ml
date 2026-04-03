type r = { mutable n : int [@atomic] }
let _ = fun (r : r) -> [%atomic.loc r.n]

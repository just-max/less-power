
module A__B = struct
  type t
end

type 'a s = C__D of 'a | D__F

let this__is__ok = C__D "<-- but this is not"

module type Cant__Do__This = sig val this__is__ok__too : int end

let x : A__B.t s = failwith "oh no"

module _ = struct
  module M1 = struct
    include Stdlib__Array
  end

  module M2 = struct
    open Stdlib__Array
  end
  
  module M3 (A : module type of Stdlib.Array) = struct end
  module M4 = M3 (Stdlib__Array)

  module M5 (A : module type of Stdlib__Array) = struct end
  module M6 = M5 (Stdlib__Array)

  let x =
    let v = 3 in
    let open Stdlib__Array in
    v + 3

  let y = Stdlib__Array.append
end

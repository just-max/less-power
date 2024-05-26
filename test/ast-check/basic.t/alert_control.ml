let x = 1 [@@alert "-important_alert"]

[@@@alert "-crazy_important_alert"]

module M = struct
  let foo =
    let _ = 17 in
    let [@alert "--even_nested"] x = 42 in
    x + 45
end

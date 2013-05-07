let (|>) x f = f x           (* left associative *)
let (>>) f g x = g (f x)     (* left associative *)
let id x = x
let const c x = c
let split f g = fun x -> (f x, g x)


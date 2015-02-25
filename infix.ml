let (|>) x f = f x           (* function application; left associative *)
let (>>) f g x = g (f x)     (* function composition; left associative *)
let id x = x
let const c x = c
let split f g = fun x -> (f x, g x)
let nop () = ()


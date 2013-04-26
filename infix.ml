let (|>) x f = f x           (* left associative *)
let (>>) f g x = g (f x)     (* left associative *)

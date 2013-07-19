open Fold

val and_reducer: (bool, bool, bool) red
val or_reducer: (bool, bool, bool) red
val forall: ('a -> bool) -> 'a col -> bool
val exists: ('a -> bool) -> 'a col -> bool

val max_reducer: ('a -> 'a -> int) -> ('a, 'a option, 'a option) red
val min_reducer: ('a -> 'a -> int) -> ('a, 'a option, 'a option) red

val first: ('a, 'a option, 'a option) red
val last: ('a, 'a option, 'a option) red
val to_list: ('a, 'a list, 'a list) red
val to_bag: ('a, 'a list, 'a list) red

module type NUM = sig

  type t

  val zero: t
  val one: t
  val add: t -> t -> t
  val mul: t -> t -> t
end

module type NumRed = sig

  type t

  val sum: (t, t, t) red
  val product: (t, t, t) red
  val count: ('a, t, t) red
  val square_sum: (t, t, t) red

end

module Num(N: NUM) : NumRed with type t = N.t

module Int : NumRed with type t = int
module Float : NumRed with type t = float
module BigInt : NumRed with type t = Big_int.big_int

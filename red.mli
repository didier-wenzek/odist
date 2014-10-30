(** Reducers *)

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

module type SET = sig
  include Set.S

  val union_reducer : (elt, t, t) red
  val items: t -> elt col
end

module SetRed(S: Set.S) : SET with type t = S.t
module MakeSetRed(E: Set.OrderedType) : SET with type elt = E.t

module type MAP = sig
  include Map.S

  val grouping_with: ('a,'b,'b) red -> (key * 'a, 'b t, 'b t) red
  val pairs: 'a t -> (key*'a) col
end

module MapRed(M: Map.S) : MAP 
module MakeMapRed(E: Map.OrderedType) : MAP with type key = E.t

module type NUM = sig
  type t

  val zero: t
  val one: t
  val add: t -> t -> t
  val mul: t -> t -> t
end

module type NUMRED = sig
  include NUM

  val sum: (t, t, t) red
  val product: (t, t, t) red
  val count: ('a, t, t) red
  val square_sum: (t, t, t) red
end

module NumRed(N: NUM) : NUMRED with type t = N.t

module Int : NUMRED with type t = int
module Float : NUMRED with type t = float
module BigInt : NUMRED with type t = Big_int.big_int
module Int32 : NUMRED with type t = int32
module Int64 : NUMRED with type t = int64
module Nativeint : NUMRED with type t = nativeint
module Complex : NUMRED with type t = Complex.t

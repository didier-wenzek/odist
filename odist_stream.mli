type ('a,'b,'c) sink = {
  init: unit -> 'b * (unit->unit);
  push: 'b -> 'a -> 'b;
  term: 'b -> 'c;
  full: ('b -> bool) option;
}

type 'a sfoldable = { sfold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b }
type 'a tfoldable = { tfold: 'b 'c. ('a,'b,'c) sink -> 'c }
type 'a src =
  | Stream of 'a sfoldable
  | Transf of 'a tfoldable

type ('a,'b,'c,'cs) red = {
  reduce: ('a,'b,'cs) sink;
  collect: 'a src -> 'c src;
}

val stream: ('a,'b,'c) sink -> 'a src -> 'c
val fold: ('b -> 'a -> 'b) -> 'b -> 'a src -> 'b
val reduce: ('a,'b,'c,'cs) red -> 'a src -> 'cs
val collect: ('a,'b,'c,'cs) red -> 'a src -> 'c src

val take: int -> 'a src -> 'a src
val take_while: ('a -> bool) -> 'a src -> 'a src
val drop: int -> 'a src -> 'a src
val drop_while: ('a -> bool) -> 'a src -> 'a src

val tee: ('a,'b,'c) sink -> 'a src -> 'a src 
val integers: int src

val monoid: 'a -> ('a->'a->'a) -> ('a,'a,'a,'a) red
val display_current: (string,unit,unit) sink

val map: ('a->'b) -> 'a src -> 'b src
val filter: ('a->bool) -> 'a src -> 'a src
val flatmap: ('a->'b src) -> 'a src -> 'b src
val unnest: ('a->'b src) -> 'a src -> ('a*'b) src

val of_single: 'a -> 'a src
val of_option: 'a option -> 'a src

val to_list: ('a,'a list,'a,'a list) red
val of_list: 'a list -> 'a src

module type SET = sig
  include Set.S

  val to_set : (elt, t, elt, t) red
  val of_set: t -> elt src
  val collecting_unique: (elt,'a,'b) sink -> (elt,t*'a,'b) sink
  val collect_unique: elt src -> elt src
end

module SetRed(S: Set.S): SET with type t = S.t and type elt = S.elt

module MakeSetRed(E: Set.OrderedType) : SET with type elt = E.t


(** Abstraction of dataset processing using union fold. *)

(** A reducer of type [('a,'b','c) red] abstracts a reduction operation :
  - over a stream of ['a] items,
  - which are aggregated into some ['b] value,
  - bringing a final outcome of type ['c].

  The aggregate values may be statefull and are built
  - starting from empty values (or buffers if there are effects)
  - to be filled and combined using the [append] and [merge] functions of the reducer
  - and transformed into a final outcome using the [result] function.
*)
type ('a,'b,'c) red = {
  empty: unit -> 'b;
  append: 'b -> 'a -> 'b;
  merge: 'b -> 'b -> 'b;
  result: 'b -> 'c;
  maximum: ('b -> bool) option;
}

(** Collection type.

  A dataset is only defined indirectly by the ability to fold its content using a reducer.
*)
type 'a col = {
  fold: 'b 'c. ('a,'b,'c) red -> 'b  -> 'b;
}

(** A monoid is the primary form of reducers.

  It is built around an associative operation with an identity.
*)
type 'a monoid = ('a,'a,'a) red

(* An action wraps a statefull and/or effectfull system. *)
type ('a,'s,'b) action = {
  init: unit -> 's;
  act: 'a -> 's -> 's;
  term: 's -> 'b;
}

(** [reduce red col] reduces the collection using the reducer. *)
val reduce: ('a,'b,'c) red -> 'a col -> 'c
val fold: ('a -> 'b) -> ('b,'c,'d) red -> 'a col -> 'd

val map: ('a -> 'b) -> 'a col -> 'b col
val flatmap: ('a -> 'b col) -> 'a col -> 'b col
val unnest: ('a -> 'b col) -> 'a col -> ('a*'b) col
val filter: ('a -> bool) -> 'a col -> 'a col
val col_product: 'a col -> 'b col -> ('a -> 'b -> 'c) -> 'c col

val monoid: 'a -> ('a -> 'a -> 'a) -> 'a monoid
val opt_monoid: ('a -> 'a -> 'a) -> ('a, 'a option, 'a option) red
val col_monoid: 'a -> ('a -> 'b -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'c) -> ('b, 'a, 'c) red
val with_maximum: 'b -> ('a,'b,'c) red -> ('a,'b,'c) red
val with_maximum_check: ('b -> bool) -> ('a,'b,'c) red -> ('a,'b,'c) red

val mapping: ('a -> 'b) -> ('b,'c,'d) red -> ('a,'c,'d) red
val flatmapping: ('a -> 'b col) -> ('b,'c,'d) red -> ('a,'c,'d) red
val unnesting: ('a -> 'b col) -> ('a*'b,'c,'d) red -> ('a,'c,'d) red
val filtering: ('a -> bool) -> ('a,'b,'c) red -> ('a,'b,'c) red
val pair_reducer: ('a,'b,'c) red -> ('a,'d,'e) red -> ('a, 'b*'d, 'c*'e) red
val returning: ('c -> 'd) -> ('a,'b,'c) red -> ('a,'b,'d) red

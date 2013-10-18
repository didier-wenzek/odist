(** Abstraction of dataset processing using union fold.

  A dataset is only defined indirectly by the ability to fold its content using a reducer
  which brings:
  - an empty initial aggregate,
  - a function to inject one item into an aggregate,
  - a function to merge two aggregates,
  - a function to extract some final result from the aggregate.


*)

type ('a,'b,'c) red = {
  empty: 'b;
  append: 'b -> 'a -> 'b;
  merge: 'b -> 'b -> 'b;
  result: 'b -> 'c;
  absorber: 'b option;
}
(** A reducer abstracts a reduction operation :
  - it takes ['a] items,
  - it aggregates these items into some ['b] value,
  - it builds a final ['c] value from the aggregate.
*)

type 'a monoid = ('a,'a,'a) red
(** A monoid is the primary form of reducers.
  It is built around an associative operation with an identity.
*)

type 'a option_monoid = ('a, 'a option, 'a option) red
(**
*)

type 'a col = {
  fold: 'b. ('b -> 'a -> 'b) -> ('b -> 'b -> 'b) -> 'b  -> 'b;
}
(** Collection type. *)

(** [reduce red col] reduces the collection using the reducer. *)
val reduce: ('a,'b,'c) red -> 'a col -> 'c
val fold: ('a -> 'b) -> ('b,'c,'d) red -> 'a col -> 'd

val map: ('a -> 'b) -> 'a col -> 'b col
val flatmap: ('a -> 'b col) -> 'a col -> 'b col
val unnest: ('a -> 'b col) -> 'a col -> ('a*'b) col
val filter: ('a -> bool) -> 'a col -> 'a col
val col_product: 'a col -> 'b col -> ('a -> 'b -> 'c) -> 'c col

val monoid: 'a -> ('a -> 'a -> 'a) -> 'a monoid
val opt_monoid: ('a -> 'a -> 'a) -> 'a option_monoid
val col_monoid: 'a -> ('a -> 'b -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'c) -> ('b, 'a, 'c) red
val with_absorber: 'b -> ('a,'b,'c) red -> ('a,'b,'c) red

val red_map: ('a -> 'b) -> ('b,'c,'d) red -> ('a,'c,'d) red
val red_flatmap: ('a -> 'b col) -> ('b,'c,'d) red -> ('a,'c,'d) red
val red_unnest: ('a -> 'b col) -> ('a*'b,'c,'d) red -> ('a,'c,'d) red
val red_filter: ('a -> bool) -> ('a,'b,'c) red -> ('a,'b,'c) red
val red_product: ('a,'b,'c) red -> ('a,'d,'e) red -> ('c -> 'e -> 'f) -> ('a, 'b*'d, 'f) red

type ('a,'b) col_monoid = ('a, 'b, 'a col) red
(** Abstraction of dataset processing using union fold. *)

(** A reducer of type [('a,'b,'c) red] abstracts a reduction operation :
  - over a stream of ['a] items,
  - which are aggregated into some ['b] value,
  - bringing a final outcome of type ['c].

  The aggregate values are built
  - starting from empty values which are
  - filled and combined using the [append] and [merge] functions of the reducer
  - and transformed into a final outcome using the [result] function.

  The following rules (aka monoid rules) must applies:
  - the [merge] operation must be associative,
  - and an empty value must be the left and right [merge] identity.

  Note, that an empty value is introduced using a function.
  So [append] and [merge] may be done in place, using buffers rather persistent values.
  This is safe since such a buffer will only be used within the scope of a single fold operation,
  where the arguments of the [append] and [merge] functions will never be reused after a call.

  A second optimization is to use an [append] function,
  rather a [single] function aimed to wrap a single value [a] into an aggregate.

     append m x = merge m (single x)
     single x = append empty x
*)
type ('a,'b,'c) red = {
  empty: unit -> 'b;
  append: 'b -> 'a -> 'b;
  merge: 'b -> 'b -> 'b;
  result: 'b -> 'c;
  maximum: ('b -> bool) option;
}

(** A value of type ['a col] abstract a collection of ['a] items.

  A dataset is only defined indirectly by the ability to fold its content,
  - either using a [sfold] function à la [List.fold_left]
  - or as grouping of sub collections to be processed independently.
*)
type 'a sfoldable = { sfold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b }
type 'a pfoldable = { pfold: 'b 'c. ('a -> 'b sfoldable) -> ('c -> 'b -> 'c) -> 'c -> 'c }
type 'a col = Stream of 'a sfoldable | Parcol of 'a col pfoldable

(** [reduce red col] reduces the collection using the reducer. *)
val reduce: ('a,'b,'c) red -> 'a col -> 'c

val to_sfoldable : 'a col -> 'a sfoldable
val fold: ('b -> 'a -> 'b) -> 'b -> 'a col -> 'b

val sfold: ('b -> 'a -> 'b) -> 'b -> 'a sfoldable -> 'b
val smap: ('a -> 'b) -> 'a sfoldable -> 'b sfoldable
val sfilter: ('a -> bool) -> 'a sfoldable -> 'a sfoldable

val map: ('a -> 'b) -> 'a col -> 'b col
val flatmap: ('a -> 'b col) -> 'a col -> 'b col
val unnest: ('a -> 'b col) -> 'a col -> ('a*'b) col
val filter: ('a -> bool) -> 'a col -> 'a col
val col_product: 'a col -> 'b col -> ('a -> 'b -> 'c) -> 'c col

(** A monoid is the primary form of reducers.

  It is built around an associative operation with an identity.
*)
type 'a monoid = ('a,'a,'a) red

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

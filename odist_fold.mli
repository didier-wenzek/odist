(** Abstraction of dataset processing using union fold. *)

(** Streams are used as the basis for all dataset processing. *)
type 'a sfoldable = 'a Odist_stream.src

(** A monoid of type [('m,'a) colmonoid] 
    defines how to reduce the content of a collection of ['a] items into some ['m] value.

  A monoid is built around an associative operation [merge] with an identity [empty] value.

  The function [add] is used to insert an item into a monoid value.
  And the function [items] unfolds all the items inserted into a monoid value.

  Note, that an empty value is introduced using a function.
  So [add] and [merge] may be done in place, using buffers rather persistent values.
  This is safe, since such a buffer will only be used within the scope of a single fold operation,
  where the arguments of the [add] and [merge] functions will never be reused after a call.

  A second optimization is to use an [add] function,
  rather a [single] function aimed to wrap a single value [a] into an aggregate.

     let add m x = merge m (single x)
     let single x = add empty x

  A third optimisation is provided by an optional [maximum] function,
  aimed to test whether or not more items may be inserted.
*)
type ('m,'a) colmonoid = {
  empty: unit -> 'm;
  add: 'm -> 'a -> 'm;
  merge: 'm -> 'm -> 'm;
  maximum: ('m -> bool) option;
  items: 'm -> 'a sfoldable;
}

type 'a monoid = ('a,'a) colmonoid

(** A reducer of type [('a,'m,'b,'c) red] abstracts a reduction operation :
  - over a stream of ['a] items,
  - which are aggregated into some ['m] value (which can be seen as a collection of ['b]),
  - bringing a final outcome of type ['c].

  The aggregates are built
  - starting from an empty ['m] value,
  - filled and combined using the [inject], [monoid.add] and [monoid.merge] functions
  - and transformed into a final outcome using the [result] function.
*)
type ('a,'m,'b,'c) red = {
  monoid: ('m,'b) colmonoid;
  inject: 'm -> 'a -> 'm;
  result: 'm -> 'c;
}

(** A value of type ['a col] abstract a collection of ['a] items.

  A dataset is only defined indirectly by the ability to fold its content,
  - either as a stream using a fold function à la [List.fold_left]
  - or as a group of sub collections to be processed independently
    leading to partial results to reducer further in a second step.
*)
type 'a pfoldable = { pfold: 'b. ('a sfoldable -> 'b sfoldable) -> 'b sfoldable }
type 'a col = Stream of 'a sfoldable | Parcol of 'a pfoldable

(** [reduce red col] reduces the collection using the reducer. *)
val reduce: ('a,'m,'b,'c) red -> 'a col -> 'c

val to_stream : 'a col -> 'a Odist_stream.src
val fold: ('b -> 'a -> 'b) -> 'b -> 'a col -> 'b
val collect_stream: ('m, 'b) colmonoid -> ('m -> 'a -> 'm) -> 'a Odist_stream.src -> 'b Odist_stream.src

val map: ('a -> 'b) -> 'a col -> 'b col
val flatmap: ('a -> 'b col) -> 'a col -> 'b col
val unnest: ('a -> 'b col) -> 'a col -> ('a*'b) col
val filter: ('a -> bool) -> 'a col -> 'a col
val col_product: 'a col -> 'b col -> ('a -> 'b -> 'c) -> 'c col

val monoid: 'a -> ('a -> 'a -> 'a) -> ('a,'a,'a,'a) red
val opt_monoid: ('a -> 'a -> 'a) -> ('a, 'a option, 'a, 'a option) red
val col_monoid: 'a -> ('a -> 'b -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'b sfoldable) -> ('b, 'a, 'b, 'a) red
val with_maximum: 'b -> ('a,'b,'c,'cs) red -> ('a,'b,'c,'cs) red
val with_maximum_check: ('b -> bool) -> ('a,'b,'c,'cs) red -> ('a,'b,'c,'cs) red

val reducer_of_monoid: ('m,'a) colmonoid -> ('a,'m,'a,'m) red
val mapping: ('a -> 'b) -> ('b,'cs,'c,'d) red -> ('a,'cs,'c,'d) red
val flatmapping: ('a -> 'b col) -> ('b,'cs,'c,'d) red -> ('a,'cs,'c,'d) red
val unnesting: ('a -> 'b col) -> ('a*'b,'cs,'c,'d) red -> ('a,'cs,'c,'d) red
val filtering: ('a -> bool) -> ('a,'cs,'c,'d) red -> ('a,'cs,'c,'d) red
val pair_monoid: ('m,'a) colmonoid -> ('n,'b) colmonoid -> ('m*'n, 'm*'n) colmonoid
val pair_reducer: ('a,'bs,'b,'c) red -> ('a,'ds,'d,'e) red -> ('a, 'bs*'ds,'bs*'ds,'c*'e) red
val returning: ('c -> 'd) -> ('a,'bs,'b,'c) red -> ('a,'bs,'b,'d) red

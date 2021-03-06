(** Reducers *)

open Odist_fold

val and_reducer: (bool, bool, bool, bool) red
val or_reducer: (bool, bool, bool, bool) red
val forall: ('a -> bool) -> 'a col -> bool
val exists: ('a -> bool) -> 'a col -> bool

val max_reducer: ('a -> 'a -> int) -> ('a, 'a option, 'a, 'a option) red
val min_reducer: ('a -> 'a -> int) -> ('a, 'a option, 'a, 'a option) red

val first: ('a, 'a option, 'a, 'a option) red
val last: ('a, 'a option, 'a, 'a option) red
val taking: int -> ('a,'m,'b,'c) red -> ('a,int*'m,int*'m,'c) red
val partition: ('a -> bool) -> ('a,'m,'b,'c) red -> ('a,'n,'d,'e) red -> ('a,'m*'n, 'm * 'n, 'c*'e) red

val to_string_buffer: int -> (string,Buffer.t,string,Buffer.t) red
val to_string: (string,Buffer.t,string,string) red

val to_list: ('a, 'a list, 'a, 'a list) red
val to_bag: ('a, 'a list, 'a, 'a list) red

val list_zipper: ('a, 'b, 'b, 'c) red -> ('a list, 'b list, 'b list, 'c list) red

module type SET = sig
  include Set.S

  val union_reducer : (elt, t, elt, t) red
  val items: t -> elt col
end

module SetRed(S: Set.S) : SET with type t = S.t
module MakeSetRed(E: Set.OrderedType) : SET with type elt = E.t

(** Standart Map.S module with reducer and collection capabilities.*)
module type MAP = sig
  include Map.S

  (** Use a value reducer, to build a reducer of (key,value) pairs into a map where all values associated to a shared key are reduced. *)
  val grouping_with: ('a,'b,'b,'b) red -> (key * 'a, 'b t, key * 'b, 'b t) red

  (** Use a key extractor and value reducer, to build a reducer of values into a map grouping and reducing values with a shared key. *)
  val grouping_by: ('a -> key) -> ('a,'b,'b,'b) red -> ('a, 'b t, key * 'b, 'b t) red

  (** Use a key reducer, to build a reducer of keys into a map where all occurences of a keys are grouped and reduced. *)
  val grouping: (key,'b,'b,'b) red -> (key, 'b t, key * 'b, 'b t) red

  (** The collection of all (key,value) pairs. *)
  val pairs: 'a t -> (key * 'a) col
end

module MapRed(M: Map.S) : MAP 
module MakeMapRed(E: Map.OrderedType) : MAP with type key = E.t

val array_reducer: ('a,'m,'b,'c) red -> (int*'a,'m array, int*'b, 'c array) red

module type NUM = sig
  type t

  val zero: t
  val one: t
  val add: t -> t -> t
  val mul: t -> t -> t
end

module type NUMRED = sig
  include NUM

  val sum: (t, t, t, t) red
  val product: (t, t, t, t) red
  val count: ('a, t, t, t) red
  val square_sum: (t, t, t, t) red
end

module NumRed(N: NUM) : NUMRED with type t = N.t

module Int : NUMRED with type t = int
module Float : NUMRED with type t = float
module BigInt : NUMRED with type t = Big_int.big_int
module Int32 : NUMRED with type t = int32
module Int64 : NUMRED with type t = int64
module Nativeint : NUMRED with type t = nativeint
module Complex : NUMRED with type t = Complex.t

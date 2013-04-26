open Fold
open Infix

let and_reducer = monoid true (&&) |> with_absorber false
let or_reducer = monoid false (||) |> with_absorber true
let forall p = fold p and_reducer
let exists p = fold p or_reducer

let max_reducer compare =
  let max a b = if compare a b >= 0 then a else b
  in opt_monoid max

let min_reducer compare =
  let min a b = if compare a b >= 0 then a else b
  in opt_monoid min

let first: 'a -> 'a -> 'a = fun a b -> a
let first = opt_monoid first

let last =
  let last a b = b
  in opt_monoid last

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
  (* val count: ('a, t, t) red *)
  val square_sum: (t, t, t) red

end

module Num(N: NUM) = struct

  type t = N.t

  let sum = monoid N.zero N.add

  let product = monoid N.one N.mul |> with_absorber N.zero

  let count = red_map (fun _ -> N.one) sum

  let square_sum = red_map (fun x -> N.mul x x) sum

end

module CamlInt = struct
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let mul = ( * )
end
module Int = Num(CamlInt)

module CamlFlot = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let add = (+.)
  let mul = ( *. )
end
module Float = Num(CamlFlot)

module NumBigInt = struct
  type t = Big_int.big_int
  let zero = Big_int.zero_big_int
  let one = Big_int.unit_big_int
  let add = Big_int.add_big_int
  let mul = Big_int.mult_big_int
end
module BigInt = Num(NumBigInt)

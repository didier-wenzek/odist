open Fold
open Infix

let and_reducer = monoid true (&&) |> with_maximum_check not
let or_reducer = monoid false (||) |> with_maximum_check id
let forall p = fold p and_reducer
let exists p = fold p or_reducer

let max_reducer compare =
  let max a b = if compare a b >= 0 then a else b
  in opt_monoid max

let min_reducer compare =
  let min a b = if compare a b >= 0 then a else b
  in opt_monoid min

let first =
  let append a b = match a with
    | None -> Some b
    | _ -> a
  in
  let merge a b = match a with
    | None -> b
    | _ -> a
  in
  {
    empty = (fun () -> None);
    append = append;
    merge = merge;
    result = id;
    maximum = Some (fun x -> match x with None -> false | _ -> true);
  }

let last =
  let append a b = Some b
  in
  let merge a b = match b with
    | None -> a
    | _ -> b
  in
  {
    empty = (fun () -> None);
    append = append;
    merge = merge;
    result = id;
    maximum = None;
  }

let taking n reducer =
  {
     empty = (fun () -> (0,reducer.empty ()));
     result = (fun (_,r) -> reducer.result r);
     append = (fun (c,xs) x -> (c+1, reducer.append xs x));
     merge = (fun (c,xs) (d,ys) -> (c+d, reducer.merge xs ys)); (* FIXME: how to avoid taking more than n items. *)
     maximum = Some (fun (c,_) -> c >= n);
  }

let to_list =
  {
    empty = (fun () -> []);
    append = (fun xs x -> x::xs);
    merge = (fun xs ys -> ys @ xs);
    result = (fun xs -> List.rev xs);
    maximum = None;
  }

let to_bag =
  {
    empty = (fun () -> []);
    append = (fun xs x -> x::xs);
    merge = List.rev_append;
    result = id;
    maximum = None;
  }

module type SET = sig
  include Set.S

  val union_reducer : (elt, t, t) red
  val items: t -> elt col
end

module SetRed(S: Set.S) = struct
  include S

  let union_reducer =
  {
    empty = const S.empty;
    append = (fun xs x -> S.add x xs);
    merge = S.union;
    result = id;
    maximum = None;
  }

  let items xs =
  {
     fold = (fun red acc -> S.fold (fun x xs -> red.append xs x) xs acc);
  }
end

module MakeSetRed(E: Set.OrderedType) = SetRed(Set.Make(E))

module type MAP = sig
  include Map.S

  val grouping_with: ('a,'b,'b) red -> (key * 'a, 'b t, 'b t) red
  val grouping_by: ('a -> key) -> ('a,'b,'b) red -> ('a, 'b t, 'b t) red
  val grouping: (key,'b,'b) red -> (key, 'b t, 'b t) red
  val pairs: 'a t -> (key * 'a) col
end

module MapRed(M: Map.S) = struct
  include M

  let grouping_with value_reducer =
    let get m k = try M.find k m with Not_found -> value_reducer.empty () in
    let value_merger k oa ob = match (oa,ob) with
      | (None, _) -> ob
      | (_, None) -> oa
      | (Some a, Some b) -> Some (value_reducer.merge a b)
    in
    {
      empty = (fun () -> M.empty);
      append = (fun m (k,v) -> let v' = get m k in let v'' = value_reducer.append v' v in M.add k v'' m);
      merge = M.merge value_merger;
      result = id;
      maximum = None;
    }

  let grouping_by k reducer = mapping (fun x -> (k x,x)) (grouping_with reducer) 
  let grouping reducer = mapping (fun x -> (x,x)) (grouping_with reducer)

  let pairs m =
    {
      fold = (fun red acc -> M.fold (fun k v acc -> red.append acc (k,v)) m acc);
    }
end

module MakeMapRed(E: Map.OrderedType) = MapRed(Map.Make(E))

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

module NumRed(N: NUM) = struct

  include N

  let sum = monoid N.zero N.add

  let product = monoid N.one N.mul |> with_maximum N.zero

  let count = {
    empty = (fun () -> N.zero);
    append = (fun c x -> N.add c N.one);
    merge = N.add;
    result = id;
    maximum = None;
  }

  let square_sum = mapping (fun x -> N.mul x x) sum
end

module CamlInt = struct
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let mul = ( * )
end
module Int = NumRed(CamlInt)

module CamlFloat = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let add = (+.)
  let mul = ( *. )
end
module Float = NumRed(CamlFloat)

module NumBigInt = struct
  type t = Big_int.big_int
  let zero = Big_int.zero_big_int
  let one = Big_int.unit_big_int
  let add = Big_int.add_big_int
  let mul = Big_int.mult_big_int
end
module BigInt = NumRed(NumBigInt)

module Int32 = NumRed(Int32)
module Int64 = NumRed(Int64)
module Nativeint = NumRed(Nativeint)
module Complex = NumRed(Complex)

open Fold
open Infix

let and_reducer = monoid true (&&) |> with_maximum_check not
let or_reducer = monoid false (||) |> with_maximum_check id
let forall p = map p >> reduce and_reducer
let exists p = map p >> reduce or_reducer

let max_reducer compare =
  let max a b = if compare a b >= 0 then a else b
  in opt_monoid max

let min_reducer compare =
  let min a b = if compare a b >= 0 then a else b
  in opt_monoid min

let first =
  let add a b = match a with
    | None -> Some b
    | _ -> a
  in
  let merge a b = match a with
    | None -> b
    | _ -> a
  in
  {
    monoid = {
      empty = (fun () -> None);
      add = add;
      merge = merge;
      maximum = Some (function None -> false | _ -> true);
      items = (fun o -> Odist_stream.of_option o);
    };
    inject = add;
    result = id;
  }

let last =
  let add a b = Some b in
  let merge a b = match b with
    | None -> a
    | _ -> b
  in
  {
    monoid = {
      empty = (fun () -> None);
      add = add;
      merge = merge;
      maximum = None;
      items = Odist_stream.of_option;
    };
    inject = add;
    result = id;
  }

(* FIXME: how to avoid taking more than n items. *)
let taking n reducer =
  let counter = monoid 0 (+) |> mapping (fun x -> 1) in
  pair_reducer counter reducer |> returning snd |> with_maximum_check (fun (c,_) -> c >= n)

let partition p true_red false_red =
  let inject (ts,fs) x = if p x then (true_red.inject ts x, fs) else (ts, false_red.inject fs x) in
  let pair_result (ts,fs) = (true_red.result ts, false_red.result fs) in
  {
    monoid = pair_monoid true_red.monoid false_red.monoid;
    inject = inject;
    result = pair_result;
  }

let to_string_buffer size =
  reducer_of_monoid {
    empty = (fun () -> Buffer.create size);
    add = (fun buf str -> Buffer.add_string buf str; buf);
    merge = (fun buf str -> Buffer.add_buffer buf str; buf);
    maximum = None;
    items = Buffer.contents >> Odist_stream.of_single;
  }

let to_string = to_string_buffer 16 |> returning Buffer.contents

let to_list =
  let add xs x = x::xs in
  {
    monoid = {
      empty = (fun () -> []);
      add = add;
      merge = (fun xs ys -> ys @ xs);
      maximum = None;
      items = (fun xs -> List.rev xs |> Odist_stream.of_list);
    };
    inject = add;
    result = (fun xs -> List.rev xs);
  }

let to_bag =
  let add xs x = x::xs in
  {
    monoid = {
      empty = (fun () -> []);
      add = add;
      merge = (fun xs ys -> List.rev_append xs ys);
      maximum = None;
      items = Odist_stream.of_list;
    };
    inject = add;
    result = id;
  }

module type SET = sig
  include Set.S

  val union_reducer : (elt, t, elt, t) red
  val items: t -> elt col
end

module SetRed(S: Set.S) = struct
  include S

  let to_sfoldable xs =
    Odist_stream.Stream {
      Odist_stream.sfold = (fun red acc -> S.fold (fun x xs -> red xs x) xs acc);
    }

  let union_reducer =
    reducer_of_monoid {
      empty = const S.empty;
      add = (fun xs x -> S.add x xs);
      merge = S.union;
      maximum = None;
      items = to_sfoldable;
    }

  let items xs = Stream (to_sfoldable xs)
end

module MakeSetRed(E: Set.OrderedType) = SetRed(Set.Make(E))

module type MAP = sig
  include Map.S

  val grouping_with: ('a,'b,'b,'b) red -> (key * 'a, 'b t, key * 'b, 'b t) red
  val grouping_by: ('a -> key) -> ('a,'b,'b,'b) red -> ('a, 'b t, key * 'b, 'b t) red
  val grouping: (key,'b,'b,'b) red -> (key, 'b t, key * 'b, 'b t) red
  val pairs: 'a t -> (key * 'a) col
end

module MapRed(M: Map.S) = struct
  include M

  let to_sfoldable kvs =
    Odist_stream.Stream {
      Odist_stream.sfold = (fun red acc -> M.fold (fun k v acc -> red acc (k,v)) kvs acc);
    }

  let grouping_with value_reducer =
    let m_reducer = value_reducer.monoid in
    let get m k = try M.find k m with Not_found -> m_reducer.empty () in
    let value_inserter inject m (k,v) = let v' = get m k in let v'' = inject v' v in M.add k v'' m in
    let value_merger k oa ob = match (oa,ob) with
      | (None, _) -> ob
      | (_, None) -> oa
      | (Some a, Some b) -> Some (m_reducer.merge a b)
    in
    {
      monoid = {
        empty = (fun () -> M.empty);
        add = value_inserter m_reducer.add;
        merge = M.merge value_merger;
        maximum = None;
        items = to_sfoldable;
      };
      inject = value_inserter value_reducer.inject;
      result = id;
    }

  let grouping_by k reducer = mapping (fun x -> (k x,x)) (grouping_with reducer) 
  let grouping reducer = mapping (fun x -> (x,x)) (grouping_with reducer)

  let pairs kvs = Stream (to_sfoldable kvs);
end

module MakeMapRed(E: Map.OrderedType) = MapRed(Map.Make(E))

let stream_of_array_i items a =
  let fold red =
    let n = Array.length a in
    let get = Array.get a >> items in
    let red_i i s x = red s (i,x) in
    let rec loop i s =
      if i = n then s
      else
        let s' = Odist_stream.fold (red_i i) s (get i) in
        loop (i+1) s'
    in loop 0 
  in
  Odist_stream.Stream { Odist_stream.sfold = fold }

let array_reducer n red =
  let monoid = red.monoid in
  let empty () = Array.init n (fun _ -> monoid.empty ()) in
  let dispatch add a (i,x) =
    let i' = i mod n in
    let x' = add (Array.get a i') x in
    Array.set a i' x'; a
  in
  let merge a b =
    let update i x =
      let x' = monoid.merge x (Array.get b i)
      in Array.set a i x'
    in
    Array.iteri update a; a
  in
  {
    monoid = {
      empty = empty;
      add = dispatch monoid.add;
      merge = merge;
      maximum = None; (* TODO *)
      items = stream_of_array_i monoid.items
    };
    inject = dispatch red.inject;
    result = Array.map red.result;
  }

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

module NumRed(N: NUM) = struct
  include N

  let sum = monoid N.zero N.add
  let product = monoid N.one N.mul |> with_maximum N.zero
  let count = { sum with inject = (fun c _ -> N.add c N.one) }
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

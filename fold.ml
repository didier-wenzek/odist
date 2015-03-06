open Util
open Infix

type 'a sfoldable = 'a Odist_stream.src
type 'a pfoldable = { pfold: 'b. ('a sfoldable -> 'b sfoldable) -> 'b sfoldable }
type 'a col = Stream of 'a sfoldable | Parcol of 'a pfoldable

type ('m,'a) colmonoid = {
  empty: unit -> 'm;
  add: 'm -> 'a -> 'm;
  merge: 'm -> 'm -> 'm;
  maximum: ('m -> bool) option;
  items: 'm -> 'a sfoldable;
}

type 'a monoid = ('a,'a) colmonoid

type ('a,'m,'b,'c) red = {
  monoid: ('m,'b) colmonoid;
  inject: 'm -> 'a -> 'm;
  result: 'm -> 'c;
}

let stream_append red =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let seed = red.monoid.empty () in (seed,nop));
    Odist_stream.push = red.inject;
    Odist_stream.term = red.result;
    Odist_stream.full = red.monoid.maximum;
  }

let collect_stream monoid inject =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let seed = monoid.empty () in (seed,nop));
    Odist_stream.push = inject;
    Odist_stream.term = monoid.items;
    Odist_stream.full = monoid.maximum;
  }

let stream_merge red =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let seed = red.monoid.empty () in (seed,nop));
    Odist_stream.push = red.monoid.add;
    Odist_stream.term = red.result;
    Odist_stream.full = red.monoid.maximum;
  }

let reduce red = function
  | Stream xs -> stream_append red xs
  | Parcol xss -> xss.pfold (collect_stream red.monoid red.inject) |> stream_merge red

let pmap f xss = { pfold = (fun g -> xss.pfold (fun xs -> g (f xs))) }
let map f = function
  | Stream xs -> Stream (Odist_stream.map f xs)
  | Parcol xss -> Parcol (pmap (Odist_stream.map f) xss)

let filter p = function
  | Stream xs -> Stream (Odist_stream.filter p xs)
  | Parcol xss -> Parcol (pmap (Odist_stream.filter p) xss)

let to_stream = function
  | Stream xs -> xs
  | Parcol xss -> xss.pfold id

let fold comb seed col = to_stream col |> Odist_stream.fold comb seed 

let sflatmap f xs =
  Odist_stream.Stream {
    Odist_stream.sfold = (fun comb seed -> Odist_stream.fold (fun acc x -> fold comb acc (f x)) seed xs)
  }
let flatmap f = function
  | Stream xs -> Stream (sflatmap f xs)
  | Parcol xss -> Parcol (pmap (sflatmap f) xss)

let sunnest f xs =
  Odist_stream.Stream {
    Odist_stream.sfold = (fun comb seed -> Odist_stream.fold (fun acc x -> fold (fun a i -> comb a (x,i)) acc (f x)) seed xs)
  }

let unnest f = function
  | Stream xs -> Stream (sunnest f xs)
  | Parcol xss -> Parcol (pmap (sunnest f) xss)

let col_product l_col r_col pair =
  let append_pair append l_item acc r_item = append acc (pair l_item r_item) in
  let append_pairs append acc x = fold (append_pair append x) acc r_col in
  let sproduct xs = Odist_stream.Stream {
      Odist_stream.sfold = (fun append seed -> Odist_stream.fold (append_pairs append) seed xs)
    }
  in
  let product = function
    | Stream xs -> Stream (sproduct xs)
    | Parcol xss -> Parcol (pmap sproduct xss)
  in
  product l_col

let mapping f red =
  let comb_map acc item = red.inject acc (f item) in
  {
    red with
    inject = comb_map;
  }

let filtering p red =
  let comb_filter p comb acc item = if p item then comb acc item else acc in
  {
    red with
    inject = comb_filter p red.inject;
  }

let flatmapping f red =
  let comb_flatmap acc item = fold red.inject acc (f item) in
  {
    red with
    inject = comb_flatmap;
  }

let unnesting f red =
  let inner_red item1 acc2 item2 = red.inject acc2 (item1,item2) in
  { red with
    inject = (fun acc1 item1 -> fold (inner_red item1) acc1 (f item1));
  }

let pair_monoid l_monoid r_monoid =
  let empty () = (l_monoid.empty (), r_monoid.empty ()) in
  let add (l_acc, r_acc) = function
    | Left item -> (l_monoid.add l_acc item, r_acc)
    | Right item -> (l_acc, r_monoid.add r_acc item)
  in
  let merge (l1, r1) (l2, r2) = (l_monoid.merge l1 l2, r_monoid.merge r1 r2) in
  let maximum = match (l_monoid.maximum, r_monoid.maximum) with
      | Some(l_max), Some(r_max) -> Some (fun (l_acc,r_acc) -> l_max l_acc && r_max r_acc)
      | _ -> None
  in
  {
    empty = empty;
    add = merge; (* FIXME: use add, which implies that items is of type: ('a,'b) either src *)
    merge = merge ;
    maximum = maximum;
    items = Odist_stream.of_single; (* FIXME: should emit a sequence of ('a,'b) either. *)
  }

let pair_reducer l_red r_red =
  let inject (l_acc, r_acc) item = (l_red.inject l_acc item, r_red.inject r_acc item) in
  let pair_result (l,r) = ((l_red.result l),(r_red.result r)) in
  {
    monoid = pair_monoid l_red.monoid r_red.monoid;
    inject = inject;
    result = pair_result;
  }

let returning result reducer =
  {
     reducer with
     result = reducer.result >> result
  }

let reducer_of_monoid monoid =
  {
    monoid = monoid;
    inject = monoid.add;
    result = id;
  }

let monoid zero plus =
  reducer_of_monoid {
    empty = (fun () -> zero);
    add = plus;
    merge = plus;
    maximum = None;
    items = Odist_stream.of_single;
  }

let opt_monoid comb =
  let add a b = match a with
    | None -> Some b
    | Some(a) -> Some (comb a b)
  in
  let merge a b = match a,b with
    | None, _ -> b
    | _, None -> a
    | Some(a),Some(b) -> Some (comb a b)
  in
  reducer_of_monoid {
    empty = (fun () -> None);
    add = add;
    merge = merge;
    maximum = None;
    items = Odist_stream.of_option;
  }

let with_maximum maximum red =
  {
    red with monoid = {
      red.monoid with 
      maximum = Some(fun x -> x = maximum)
    }
  }

let with_maximum_check maximum red =
  {
    red with monoid = {
      red.monoid with 
      maximum = Some(maximum)
    }
  }

let col_monoid empty add merge collect =
  reducer_of_monoid {
    empty = (fun () -> empty);
    add = add;
    merge = merge;
    maximum = None;
    items = collect;
  }


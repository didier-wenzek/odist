open Util
open Infix

type 'a sfoldable = 'a Odist_stream.src
type 'a pfoldable = { pfold: 'b. ('a sfoldable -> 'b sfoldable) -> 'b sfoldable }
type 'a col = Stream of 'a sfoldable | Parcol of 'a pfoldable

type ('a,'b,'c) red = {
  empty: unit -> 'b;
  append: 'b -> 'a -> 'b;
  merge: 'b -> 'b -> 'b;
  result: 'b -> 'c;
  maximum: ('b -> bool) option;
}

type ('a,'m,'s) action = {
  reducer: ('a,'m,'m) red;
  init: unit -> 's * (unit -> unit);
  push_item: 'a -> 's -> 's;
  push: 'm -> 's -> 's;
}

type 'a monoid = ('a,'a,'a) red

let stream_append red =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let seed = red.empty () in (seed,nop));
    Odist_stream.push = red.append;
    Odist_stream.term = red.result;
    Odist_stream.full = red.maximum;
  }

let collect_stream red =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let seed = red.empty () in (seed,nop));
    Odist_stream.push = red.append;
    Odist_stream.term = Odist_stream.of_single;
    Odist_stream.full = red.maximum;
  }

let stream_merge red =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let seed = red.empty () in (seed,nop));
    Odist_stream.push = red.merge;
    Odist_stream.term = red.result;
    Odist_stream.full = red.maximum;
  }

let reduce red = function
  | Stream xs -> stream_append red xs
  | Parcol xss -> xss.pfold (collect_stream red) |> stream_merge red

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
  let comb_map f comb acc item = comb acc (f item) in
  {
    red with
    append = comb_map f red.append
  }

let filtering p red =
  let comb_filter p comb acc item = if p item then comb acc item else acc in
  {
    red with
    append = comb_filter p red.append
  }

let flatmapping f red =
  let comb_flatmap acc item = fold red.append acc (f item) in
  {
    red with
    append = comb_flatmap
  }

let unnesting f red =
  let inner_red item1 acc2 item2 = red.append acc2 (item1,item2) in
  { red with
    append = (fun acc1 item1 -> fold (inner_red item1) acc1 (f item1));
  }

let pair_reducer l_red r_red =
  let split_append (l_acc, r_acc) item = (l_red.append l_acc item, r_red.append r_acc item) in
  let split_merge (l1, r1) (l2, r2) = (l_red.merge l1 l2, r_red.merge r1 r2) in
  let pair_result (l,r) = ((l_red.result l),(r_red.result r)) in
  {
    empty = (fun () -> (l_red.empty (), r_red.empty ()));
    append = split_append;
    merge = split_merge;
    result = pair_result;
    maximum = None;
  }

let returning result reducer =
  {
     reducer with
     result = reducer.result >> result
  }

let monoid zero plus =
  {
    empty = (fun () -> zero);
    append = plus;
    merge = plus;
    result = id;
    maximum = None;
  }

let opt_monoid comb =
  let append a b = match a with
    | None -> Some b
    | Some(a) -> Some (comb a b)
  in
  let merge a b = match a,b with
    | None, _ -> b
    | _, None -> a
    | Some(a),Some(b) -> Some (comb a b)
  in
  {
    empty = (fun () -> None);
    append = append;
    merge = merge;
    result = id;
    maximum = None;
  }

let with_maximum maximum red =
  {
    red with
    maximum = Some(fun x -> x = maximum)
  }

let with_maximum_check maximum red =
  {
    red with
    maximum = Some(maximum)
  }

let col_monoid empty append merge collect =
  {
    empty = (fun () -> empty);
    append = append;
    merge = merge;
    result = collect;
    maximum = None;
  }


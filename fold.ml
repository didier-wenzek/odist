open Util
open Infix

type 'a pfoldable = { pfold: 'b 'c. ('a -> 'b Odist_stream.src) -> ('c -> 'b -> 'c) -> 'c -> 'c }
type 'a col = Stream of 'a Odist_stream.src | Parcol of 'a col pfoldable

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

let reduce red col =
  let acc = match red.maximum with
    | None -> (
      let rec loop = function
        | Stream xs -> Odist_stream.fold red.append (red.empty ()) xs
        | Parcol xss -> xss.pfold (fun xs -> loop xs |> Odist_stream.of_single) red.merge (red.empty ())
      in loop col
    )
    | Some(maximum) -> with_return (fun return ->
      let append_or_return acc i = if maximum acc then return acc else red.append acc i in
      let merge_or_return a b = if maximum a then return a else red.merge a b in
      let rec loop = function
        | Stream xs -> Odist_stream.fold append_or_return (red.empty ()) xs
        | Parcol xss -> xss.pfold (fun xs -> loop xs |> Odist_stream.of_single) merge_or_return (red.empty ())
      in loop col
    )
  in
  red.result acc

let pmap f xss = { pfold = (fun g -> xss.pfold (fun xs -> g (f xs))) }
let rec map f = function
  | Stream xs -> Stream (Odist_stream.map f xs)
  | Parcol xss -> Parcol (pmap (map f) xss)

let rec filter p = function
  | Stream xs -> Stream (Odist_stream.filter p xs)
  | Parcol xss -> Parcol (pmap (filter p) xss)

let rec to_stream = function
  | Stream xs -> xs
  | Parcol xss -> Odist_stream.Stream {
     Odist_stream.sfold = (fun comb -> xss.pfold to_stream comb)
  }

let fold comb seed col = to_stream col |> Odist_stream.fold comb seed 

let sflatmap f xs =
  Odist_stream.Stream {
    Odist_stream.sfold = (fun comb seed -> Odist_stream.fold (fun acc x -> fold comb acc (f x)) seed xs)
  }
let rec flatmap f = function
  | Stream xs -> Stream (sflatmap f xs)
  | Parcol xss -> Parcol (pmap (flatmap f) xss)

let sunnest f xs =
  Odist_stream.Stream {
    Odist_stream.sfold = (fun comb seed -> Odist_stream.fold (fun acc x -> fold (fun a i -> comb a (x,i)) acc (f x)) seed xs)
  }

let rec unnest f = function
  | Stream xs -> Stream (sunnest f xs)
  | Parcol xss -> Parcol (pmap (unnest f) xss)

let col_product l_col r_col pair =
  let append_pair append l_item acc r_item = append acc (pair l_item r_item) in
  let append_pairs append acc x = fold (append_pair append x) acc r_col in
  let sproduct xs = Odist_stream.Stream {
      Odist_stream.sfold = (fun append seed -> Odist_stream.fold (append_pairs append) seed xs)
    }
  in
  let rec product = function
    | Stream xs -> Stream (sproduct xs)
    | Parcol xss -> Parcol (pmap product xss)
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


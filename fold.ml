open Util
open Infix

type ('a,'b,'c) red = {
  empty: 'b;
  append: 'b -> 'a -> 'b;
  merge: 'b -> 'b -> 'b;
  result: 'b -> 'c;
  maximum: ('b -> bool) option;
}

type 'a col = {
  fold: 'b 'c. ('a,'b,'c) red -> 'b  -> 'b;
}
type 'a monoid = ('a,'a,'a) red
type 'a option_monoid = ('a, 'a option, 'a option) red
type ('a,'b) col_monoid = ('a, 'b, 'a col) red

let reduce red col =
  let acc = match red.maximum with
  | None -> col.fold red red.empty
  | Some(maximum) -> with_return (fun return ->
    let append_or_return acc i = if maximum acc then return acc else red.append acc i in
    let merge_or_return a b = let m = red.merge a b in if maximum m then return m else m in
    col.fold { red with
      append = append_or_return;
      merge = merge_or_return;
    } red.empty
  )
  in red.result acc

let mapping f red =
  let comb_map f comb acc item = comb acc (f item) in
  {
    red with
    append = comb_map f red.append
  }

let map f col =
  {
    fold = (fun red -> col.fold (mapping f red));
  }

let fold f red = reduce (mapping f red)

let filtering p red =
  let comb_filter p comb acc item = if p item then comb acc item else acc in
  {
    red with
    append = comb_filter p red.append
  }

let filter p col =
  {
    fold = (fun red -> col.fold (filtering p red));
  }

let flatmapping f red =
  let comb_flatmap acc item = (f item).fold red acc in
  {
    red with
    append = comb_flatmap
  }

let flatmap f col =
  {
    fold = (fun red -> col.fold (flatmapping f red));
  }

let unnesting f red =
  let inner_red item1 = { red with
    append = (fun acc2 item2 -> red.append acc2 (item1,item2));
  }
  in { red with
    append = (fun acc1 item1 -> (f item1).fold (inner_red item1) acc1);
  }

let unnest f col =
  {
    fold = (fun red -> col.fold (unnesting f red));
  }

let col_product l_col r_col pair =
  let appending_pair red l_item = { red with
    append = (fun acc r_item -> red.append acc (pair l_item r_item));
  }
  in
  let inner_red red = { red with
     append = (fun acc l_item -> r_col.fold (appending_pair red l_item) acc);
  }
  in
  {
    fold = (fun red -> l_col.fold (inner_red red));
  }
 
let pair_reducer l_red r_red =
  let split_append (l_acc, r_acc) item = (l_red.append l_acc item, r_red.append r_acc item) in
  let split_merge (l1, r1) (l2, r2) = (l_red.merge l1 l2, r_red.merge r1 r2) in
  let pair_result (l,r) = ((l_red.result l),(r_red.result r)) in
  {
    empty = (l_red.empty, r_red.empty);
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
    empty = zero;
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
    empty = None;
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
    empty = empty;
    append = append;
    merge = merge;
    result = collect;
    maximum = None;
  }



open Util
open Infix

type ('a,'b,'c) red = {
  empty: 'b;
  append: 'b -> 'a -> 'b;
  merge: 'b -> 'b -> 'b;
  result: 'b -> 'c;
  absorber: 'b option;
}

type 'a col = {
  fold: 'b. ('b -> 'a -> 'b) -> ('b -> 'b -> 'b) -> 'b  -> 'b;
}
type 'a monoid = ('a,'a,'a) red
type 'a option_monoid = ('a, 'a option, 'a option) red
type ('a,'b) col_monoid = ('a, 'b, 'a col) red

let reduce red col =
  let acc = match red.absorber with
  | None -> col.fold red.append red.merge red.empty
  | Some(absorber) -> with_return (fun return ->
    let append_or_return acc i = if acc = absorber then return acc else red.append acc i in
    col.fold append_or_return red.merge red.empty
  )
  in red.result acc

let comb_filter p comb acc item = if p item then comb acc item else acc
let comb_map f comb acc item = comb acc (f item)
let comb_flatmap f comb merge acc item = (f item).fold comb merge acc
let comb_unnest f comb merge acc item1 =
  let pcomb acc item2 = comb acc (item1,item2) in
    (f item1).fold pcomb merge acc

let map f col =
  let fold append = col.fold (comb_map f append) in
  {
    fold = fold
  }

let flatmap f col =
  let fold append merge = col.fold (comb_flatmap f append merge) merge in
  {
    fold = fold
  }

let unnest f col =
  let fold append merge = col.fold (comb_unnest f append merge) merge in
  {
    fold = fold
  }

let filter p col =
  let fold append = col.fold (comb_filter p append) in
  {
    fold = fold
  }

let col_product l_col r_col pair =
  let append_pair append l_item acc r_item = append acc (pair l_item r_item) in
  let append_inner append merge acc l_item = r_col.fold (append_pair append l_item) merge acc in
  let fold append merge = l_col.fold (append_inner append merge) merge in
  {
    fold = fold
  }
 
let red_map f red =
  {
    red with
    append = comb_map f red.append
  }

let red_filter p red =
  {
    red with
    append = comb_filter p red.append
  }

let red_flatmap f red =
  {
    red with
    append = comb_flatmap f red.append red.merge
  }

let red_unnest f red =
  {
    red with
    append = comb_unnest f red.append red.merge
  }

let fold f red = reduce (red_map f red)

let red_product l_red r_red pair =
  let split_append (l_acc, r_acc) item = (l_red.append l_acc item, r_red.append r_acc item) in
  let split_merge (l1, r1) (l2, r2) = (l_red.merge l1 l2, r_red.merge r1 r2) in
  let pair_result (l,r) = pair (l_red.result l) (r_red.result r) in
  {
    empty = (l_red.empty, r_red.empty);
    append = split_append;
    merge = split_merge;
    result = pair_result;
    absorber = None;
  }

let monoid zero plus =
  {
    empty = zero;
    append = plus;
    merge = plus;
    result = id;
    absorber = None;
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
    absorber = None;
  }

let with_absorber absorber red =
  {
    red with
    absorber = Some(absorber)
  }

let col_monoid empty append merge collect =
  {
    empty = empty;
    append = append;
    merge = merge;
    result = collect;
    absorber = None;
  }



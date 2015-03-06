open Fold

type 'a split =
  | S of string
  | B of string * 'a * string

let fold_all_but_last comb seed = 
   let rec loop acc items =
     match items with
     | [] -> (acc, None)
     | [last] -> (acc, Some(last))
     | item::others -> loop (comb acc item) others
   in loop seed

let make_appender merge single =
  fun xs x -> merge xs (single x)

let make_prepender merge single =
  fun x xs -> merge (single x) xs

let pack_split_reducer split reducer =
  let m = reducer.monoid in
  let add = reducer.inject in
  let empty = S("") in
  let single str =
    match split str with
    | [] -> empty
    | left::tail -> (
      let middle,oright = fold_all_but_last add (m.empty ()) tail in
      match oright with
      | None -> S(left)
      | Some(right) -> B(left,middle,right)
    )
  in
  let merge2 = m.merge in
  let merge3 ma s mb = merge2 (add ma s) mb in
  let merge4 ma sa sb mb = match single (sa ^ sb) with
    | S(s) -> merge3 ma s mb
    | B(sl,sm,sr) -> merge3 ma sl (merge3 sm sr mb)
  in
  let prepend3 sa sb mb rb = match single (sa ^ sb) with
    | S(s) -> B(s,mb,rb)
    | B(sl,sm,sr) -> B(sl, merge3 sm sr mb, rb)
  in
  let append3 la ma sa sb = match single (sa ^ sb) with
    | S(s) -> B(la, ma,s)
    | B(sl,sm,sr) -> B(la, merge3 ma sl sm, sr)
  in
  let reducer_single item = add (m.empty ()) item in
  let reducer_prepend item items = m.merge (reducer_single item) items in
  let merge a b = match (a,b) with
    | S(sa),S(sb) -> single (sa ^ sb)
    | S(sa),B(lb,mb,rb) -> prepend3 sa lb mb rb
    | B(la,ma,ra),S(sb) -> append3 la ma ra sb
    | B(la,ma,ra),B(lb,mb,rb) -> B(la, merge4 ma ra lb mb, rb)
  in
  let append xs x = merge xs (single x) in
  let pack a = match a with
    | S(s) -> reducer_single s
    | B(l,mv,r) -> reducer_prepend l (add mv r) 
  in
  let result a = reducer.result (pack a) in
  let maximum = None
  in
  {
    monoid = {
      empty = (fun () -> empty);
      add = merge;
      merge = merge;
      maximum = maximum;
      items = Odist_stream.of_single;
    };
    inject = append;
    result = result;
  }

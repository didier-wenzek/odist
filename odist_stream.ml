open Odist_util
open Odist_infix

type ('a,'b,'c) sink = {
  init: unit -> 'b;
  push: 'b -> 'a -> 'b;
  term: 'b -> 'c;
  full: ('b -> bool) option;
}

type 'a sfoldable = { sfold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b }
type 'a tfoldable = { tfold: 'b 'c. ('a,'b,'c) sink -> 'c }
type 'a src =
  | Stream of 'a sfoldable
  | Transf of 'a tfoldable

type ('a,'b,'c,'cs) red = {
  reduce: ('a,'b,'cs) sink;
  collect: 'a src -> 'c src;
}

type 'a resource = unit -> ('a * (unit->unit))

let stream sink = function
  | Transf xs -> xs.tfold sink
  | Stream xs -> 
    let fold hdl = sink.term (match sink.full with
      | None -> xs.sfold sink.push hdl
      | Some(maximum) -> with_return (fun return ->
        let push_or_return acc i = if maximum acc then return acc else sink.push acc i in
        xs.sfold push_or_return hdl
      )
    ) in
    fold (sink.init ())

let stream_to sink_resource src =
  let sink,finally = sink_resource () in
  protect ~finally (stream sink) src

let init_value e = fun () -> e

let fold comb seed = function
  | Stream xs -> xs.sfold comb seed
  | Transf xs -> xs.tfold {
    init = init_value seed;
    push = comb;
    term = id;
    full = None; 
  }

let reduce red = stream red.reduce
let collect red = red.collect

(* This fail to compile !

  The tfold field value has type ('d, 'e, 'f) sink -> 'f
  which is less general than 'b 'c. ('a, 'b, 'c) sink -> 'c

  => the transformation has to be done for each stream sink adapter:
  mapping -> map, filtering -> filter, taking -> take, ...
   

  let apply_stream_adapter adapt xs =
    let fold sink = stream (adapt sink) xs in
    Transf {
       tfold = fold
    }
*)
  
(* This fails to compile too !

  This sfold value has type ('c -> 'd -> 'c) -> 'c -> 'c
  which is less general than 'b. ('b -> 'a -> 'b) -> 'b -> 'b

  => all source has to be explicitly defined.

  let of_fold fold = Stream { sfold = fold; }
*)

let take n xs =
  let adapt sink = {
    init = (fun () -> (0,sink.init ()));
    push = (fun ((i,hdl) as acc) x -> if i < n then (i+1, sink.push hdl x) else acc);
    term = (fun (_,hdl) -> sink.term hdl);
    full = match sink.full with
      | None -> Some (fun (i,_) -> i>=n)
      | Some(maximum) -> Some (fun (i,hdl) -> i>=n || maximum hdl);
  } in
  Transf {
    tfold = (fun sink -> xs |> stream (adapt sink))
  }

let take_while p xs =
  let adapt sink = {
    init = (fun () -> (true,sink.init ()));
    push = (fun (ok,hdl) x -> if ok && p x then (true, sink.push hdl x) else (false,hdl));
    term = (fun (_,hdl) -> sink.term hdl);
    full = match sink.full with
      | None -> Some (fun (ok,_) -> not ok)
      | Some(maximum) -> Some (fun (ok,hdl) -> not ok || maximum hdl);
  } in
  Transf {
    tfold = (fun sink -> xs |> stream (adapt sink))
  }

let drop n xs =
  let adapt sink = {
    init = (fun () -> (0,sink.init ()));
    push = (fun (i,hdl) x -> if i < n then (i+1, hdl) else (i,sink.push hdl x));
    term = (fun (_,hdl) -> sink.term hdl);
    full = match sink.full with
      | None -> None
      | Some(maximum) -> Some (fun (_,hdl) -> maximum hdl);
  } in
  Transf {
    tfold = (fun sink -> xs |> stream (adapt sink))
  }

let drop_while p xs =
  let adapt sink = {
    init = (fun () -> (true,sink.init ()));
    push = (fun (ok,hdl) x -> if ok && p x then (true, hdl) else (false,sink.push hdl x));
    term = (fun (_,hdl) -> sink.term hdl);
    full = match sink.full with
      | None -> None
      | Some(maximum) -> Some (fun (_,hdl) -> maximum hdl);
  } in
  Transf {
    tfold = (fun sink -> xs |> stream (adapt sink))
  }


let tee sink xs =
  let adapt sink2 = {
    init = (fun () -> (sink.init (), sink2.init ()));
    push = (fun (hdl,hdl2) x -> (sink.push hdl x, sink2.push hdl2 x));
    term = (fun (_,hdl2) -> sink2.term hdl2);
    full = match sink2.full with
      | None -> None
      | Some(maximum) -> Some (fun (_,hdl2) -> maximum hdl2);
  } in
  Transf {
    tfold = (fun sink2 -> xs |> stream (adapt sink2))
  }

let integers =
  let fold push =
    let rec loop i acc = loop (i+1) (push acc i) in
    loop 0
  in
  Stream {
    sfold = fold;
  }

let of_empty = Stream { sfold = (fun push seed -> seed) }
let of_single x = Stream { sfold = (fun push seed -> push seed x) }
let of_option = function
  | None -> of_empty
  | Some x -> of_single x

let collect_single sink = fun xs -> xs |> stream sink |> of_single

let monoid zero plus =
  let to_sink = {
    init = init_value zero;
    push = (fun x acc -> plus acc x);
    term = id;
    full = None;
  } in {
    reduce = to_sink;
    collect = collect_single to_sink;
  }

let display_current = {
  init = init_value ();
  push = (fun () str -> Printf.printf "\x1B[8D%s\r%!" str);
  term = (fun () -> Printf.printf "\n%!");
  full = None;
}

let mapping f sink = {
    sink with
    push = (fun acc x -> sink.push acc (f x))
  }

let map f xs =
  Transf {
    tfold = (fun sink -> xs |> stream (mapping f sink))
  }

let filtering p sink = {
    sink with
    push = (fun acc x -> if p x then sink.push acc x else acc)
  }

let filter q xs =
  Transf {
    tfold = (fun sink -> xs |> stream (filtering q sink))
  }

let flatmapping f sink = {
    sink with
    push = (fun acc x -> fold sink.push acc (f x))
  }

let flatmap f xs =
  Transf {
    tfold = (fun sink -> xs |> stream (flatmapping f sink))
  }

let unnesting f sink = {
    sink with
    push = (fun acc x -> fold (fun acc y -> sink.push acc (x,y)) acc (f x))
  }

let unnest f xs =
  Transf {
    tfold = (fun sink -> xs |> stream (unnesting f sink))
  }

module type SET = sig
  include Set.S

  val to_set : (elt, t, elt, t) red
  val of_set: t -> elt src
  val collecting_unique: (elt,'a,'b) sink -> (elt,t*'a,'b) sink
  val collect_unique: elt src -> elt src
end

module SetRed(S: Set.S): SET with type t = S.t and type elt = S.elt = struct
  include S

  let set_reducer =
  {
    init = init_value empty;
    push = (fun xs x -> add x xs);
    term = id;
    full = None;
  }

  let collecting_unique sink = {
    init = (fun () -> (empty,sink.init ()));
    push = (fun ((xs,hdl) as acc) x -> if not (mem x xs) then (add x xs, sink.push hdl x) else acc);
    term = (fun (_,hdl) -> sink.term hdl);
    full = match sink.full with
      | None -> None
      | Some(maximum) -> Some (fun (_,hdl) -> maximum hdl);
  }

  let collect_unique xs =
  Transf {
    tfold = (fun sink -> xs |> stream (collecting_unique sink))
  }

  let to_set =
  {
    reduce = set_reducer;
    collect = collect_unique;
  }

  let of_set xs =
  Stream {
     sfold = (fun push -> S.fold (fun x acc -> push acc x) xs);
  }

end

let list_reducer = {
  init = (fun () -> []);
  push = (fun xs x -> x::xs);
  term = (fun xs -> List.rev xs);
  full = None;
}

let to_list = {
  reduce = list_reducer;
  collect = id;
}

let of_list xs =
  let fold push =
    let rec loop xs s = match xs with
      | [] -> s
      | x::xs -> loop xs (push s x)
    in loop xs
  in
  Stream {
     sfold = fold;
  }

module MakeSetRed(E: Set.OrderedType) : SET with type elt = E.t = SetRed(Set.Make(E))


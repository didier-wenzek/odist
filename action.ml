open Fold

type ('a,'s) action = {
  init: unit -> 's;
  act: 'a -> 's -> 's;
  term: 's -> unit;
}

type 's computation = State of 's | Action of ('s -> 's)

let apply act c a = match c with
  | State s -> State (act a s)
  | Action b -> Action (fun s -> act a (b s))

let and_then f g = match (f,g) with
  | (State s, Action a) -> State (a s)
  | (Action a, Action b) -> Action (fun s -> b (a s))
  | (_,_) -> assert false

let term_with term c = match c with
  | State s -> term s
  | _ -> assert false

let actor action = 
  {
    empty = Action (fun s -> s);
    append = apply action.act;
    merge = and_then;
    result = term_with action.term;
    absorber = None;
  }

let stream action col =
  let actor = actor action in
  let state = State (action.init ()) in
  try actor.result (col.fold actor.append actor.merge state)
  with e -> actor.result state; raise e

let printer = {
  init = (fun () -> ());
  act = (fun s () -> print_string s);
  term = (fun () -> ());
}

let printer_to file = {
  init = (fun () -> open_out file);
  act = (fun s out -> output_string out s; out);
  term = close_out;
}
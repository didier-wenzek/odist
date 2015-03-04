open Fold
open Util
open Infix

type ('a,'m,'s) action = {
  reducer: ('a,'m,'m) Fold.red;
  init: unit -> 's;
  push_item: 's -> 'a -> 's;
  push: 's -> 'm -> 's;
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

let sstream sys =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let hdl = sys.init () in (hdl,fun () -> sys.term hdl));
    Odist_stream.push = sys.push_item;
    Odist_stream.term = (fun _ -> ());
    Odist_stream.full = None;
  }

let pstream sys =
  Odist_stream.stream {
    Odist_stream.init = (fun () -> let hdl = sys.init () in (hdl,fun () -> sys.term hdl));
    Odist_stream.push = sys.push;
    Odist_stream.term = (fun _ -> ());
    Odist_stream.full = None;
  }

let stream sys = function
  | Stream xs -> sstream sys xs
  | Parcol xss -> xss.pfold (collect_stream sys.reducer) |> pstream sys

let to_printer = {
  reducer = Red.to_string_buffer 64;
  init = nop;
  push_item = (fun () c -> print_string c);
  push = (fun out s -> Buffer.output_buffer stdout s);
  term = nop;
}

let to_file_printer file = {
  reducer = Red.to_string_buffer 64;
  init = (fun () -> open_out file);
  push_item = (fun out s -> output_string out s; out);
  push = (fun out s -> Buffer.output_buffer out s; out);
  term = close_out;
}

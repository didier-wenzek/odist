module Stream = Odist_stream
module Fold = Odist_fold
module Red = Odist_red
open Fold
open Odist_util

type ('a,'m,'s) action = {
  monoid: ('m,'a) Fold.colmonoid;
  resource: ('a,'s,unit) Odist_stream.sink Odist_stream.resource;
}

let sstream action = Stream.stream_to action.resource

let stream action = function
  | Stream xs -> sstream action xs
  | Parcol xss -> xss.pfold (collect_stream action.monoid action.monoid.add) |> sstream action

let to_printer =
  let nop () = () in
  let sink = Odist_stream.{
    init = nop;
    push = (fun () c -> print_string c);
    term = ignore;
    full = None;
  } in
  let close = nop in
  {
    monoid = (Red.to_string_buffer 64).monoid;
    resource = (fun () -> (sink,close));
  }

let to_file_printer file =
  let open_channel () =
    let out = open_out file in
    let close () = close_out out in
    let sink = Odist_stream.{
      init = (fun () -> ());
      push = (fun () c -> output_string out c);
      term = ignore;
      full = None;
    }
    in (sink,close)
  in
  {
    monoid = (Red.to_string_buffer 64).monoid;
    resource = open_channel;
  }

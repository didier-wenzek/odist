module Infix = Odist_infix
module Stream = Odist_stream
module Fold = Odist_fold
open Fold
open Infix

type ('a,'m,'s) action = {
  monoid: ('m,'a) Fold.colmonoid;
  system: ('a,'s,unit) Stream.sink;
}

let sstream action = Stream.stream action.system

let stream action = function
  | Stream xs -> sstream action xs
  | Parcol xss -> xss.pfold (collect_stream action.monoid action.monoid.add) |> sstream action

let to_printer = {
  monoid = (Red.to_string_buffer 64).monoid;
  system = Odist_stream.{
    init = (fun () -> ((),nop));
    push = (fun () c -> print_string c);
    term = ignore;
    full = None;
  }
}

let to_file_printer file = {
  monoid = (Red.to_string_buffer 64).monoid;
  system = Odist_stream.{
    init = (fun () -> let out = open_out file in (out,fun () -> close_out out));
    push = (fun out c -> output_string out c; out);
    term = ignore;
    full = None;
  }
}

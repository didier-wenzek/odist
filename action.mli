open Fold

type ('a,'s) action = {
  init: unit -> 's;
  act: 'a -> 's -> 's;
  term: 's -> unit;
}

val stream : ('a,'s) action -> 'a col -> unit

val printer : (string, unit) action
val printer_to : string -> (string, out_channel) action

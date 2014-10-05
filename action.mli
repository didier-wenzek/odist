(* An action wraps a statefull and/or effectfull system. *)
type ('a,'s,'b) action = {
  init: unit -> 's;
  act: 'a -> 's -> 's;
  term: 's -> 'b;
}

(* Streams the action of items of a collection over a statefull and/or effectfull system.

   [col |> stream_to action]
   starts to initialize the statefull system implied by the action, calling [action.init].
   Then each items of the collection [col] is used to act on the system,
   applying [action.act item] on the current state to get the new one.
   Finally the system resources are released using [action.term].
*)
val stream_to : ('a,'s,'b) action -> 'a Fold.col -> 'b

(* Printing action of a stream of strings on standard output. *)
val printer : (string, unit, unit) action

(* Printing action of a stream of strings on a file. *)
val printer_to : string -> (string, out_channel, unit) action

(* Buffering action of a stream of strings into a string buffer.

   [col |> stream_to (string_buffer size)] returns the concatenation of all strings of the collection [col]
   using a string buffer of the given size.
*)
val string_buffer : int -> (string, Buffer.t, string) action

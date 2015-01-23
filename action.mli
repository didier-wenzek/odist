(* An action wraps a statefull and/or effectfull system. *)
type ('a,'s,'b) action = {
  init: unit -> 's;
  act: 'a -> 's -> 's;
  term: 's -> 'b;
}

(* Streams the action of items of a collection over a statefull and/or effectfull system.

   [col |> stream action]
   starts to initialize the statefull system implied by the action, calling [action.init].
   Then each items of the collection [col] is used to act on the system,
   applying [action.act item] on the current state to get the new one.
   Finally the system resources are released using [action.term].
*)
val stream : ('a,'s,'b) action -> 'a Fold.col -> 'b

(* Printing action of a stream of strings on standard output. *)
val to_printer : (string, unit, unit) action

(* Printing action of a stream of strings into a file. *)
val to_file_printer : string -> (string, out_channel, unit) action

(* Buffering action of a stream of strings into a string buffer.

   [col |> stream (to_string_buffer size)] returns the concatenation of all strings of the collection [col]
   using a string buffer of the given size.
*)
val to_string_buffer : int -> (string, Buffer.t, string) action

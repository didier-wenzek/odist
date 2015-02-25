(* An action wraps a statefull and/or effectfull system.

   It works around a monoid and a system on which the monoid values act.

   The system is represented by a state/handler
   (provided by a call to [init ()] along a [term] aimed to release the system resources).

   The action of a value of type ['m] is provided by the function [push]
   which must be compatible with the monoid.

      (* the empty reducer value has no effect *)
      push (reducer.empty ()) = id 

      (* applying ['m] values is row is the same as applying their combination *)
      push x >> push y     =   push (red.merge x y)

      (* push_item is a short hand for pushing a single value *)
      push_item x = push (single x)

   An action exemple, is printing which is related to string concatenation.

      let print_to file = {
        reducer = monoid "" (^);
        init = (fun () -> open_out file);
        push = (fun s out -> output_string out s; out);
        push_item = (fun s out -> output_char out s; out);
        term = close_out
      }
*)
type ('a,'m,'s) action = {
  reducer: ('a,'m,'m) Fold.red;
  init: unit -> 's;
  push_item: 's -> 'a -> 's;
  push: 's -> 'm -> 's;
  term: 's -> unit;
}

(* Streams the action of items of a collection over a statefull and/or effectfull system.

   [col |> stream action]
   starts to initialize the statefull system implied by the action, calling [action.init ()].
   Then each items of the collection [col] is pushed into the system,
   applying [action.push_item item] or [action.push aggregate] on the current state to get the new one.
   Finally the system resources are released using the term function returned by [action.init ()].
*)
val stream : ('a,'m,'b) action -> 'a Fold.col -> unit
val sstream : ('a,'m,'b) action -> 'a Fold.sfoldable -> unit

(* Printing action of a stream of strings on standard output. *)
val to_printer : (string, Buffer.t, unit) action

(* Printing action of a stream of strings into a file. *)
val to_file_printer : string -> (string, Buffer.t, out_channel) action


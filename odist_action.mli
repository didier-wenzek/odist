module Fold = Odist_fold

(* An action wraps a statefull and/or effectfull system.

   It works around a monoid and a system on which the monoid values act.

   The system is represented by sink, to which values of type ['m] can be pushed.
   This push action must be compatible the monoid. I.e:

      (* the empty value has no effect *)
      system.push s (monoid.empty ()) = s 

      (* applying ['m] values is row is the same as applying their combination *)
      system.push x >> system.push y     =   system.push (monoid.merge x y)

*)
type ('a,'m,'s) action = {
  monoid: ('m,'a) Fold.colmonoid;
  system: ('a,'s,unit) Odist_stream.sink;
}

(* Streams the action of items of a collection over a statefull and/or effectfull system.

   [col |> stream action]
*)
val stream : ('a,'m,'s) action -> 'a Fold.col -> unit
val sstream : ('a,'m,'s) action -> 'a Odist_stream.src -> unit

(* Printing action of a stream of strings on standard output. *)
val to_printer : (string, Buffer.t, unit) action

(* Printing action of a stream of strings into a file. *)
val to_file_printer : string -> (string, Buffer.t, out_channel) action


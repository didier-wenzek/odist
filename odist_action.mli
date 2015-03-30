module Fold = Odist_fold

(* An action wraps a statefull and/or effectfull system.

   It works around a monoid and a resource on which the monoid values act.

   The resource is represented by sink, to which values of type ['m] can be pushed.
   This push action must be compatible the monoid. I.e:

      (* the empty value has no effect *)
      resource.push s (monoid.empty ()) = s 

      (* applying ['m] values in row is the same as applying their combination *)
      resource.push x >> resource.push y     =   resource.push (monoid.merge x y)
*)
type ('a,'m,'s) action = {
  monoid: ('m,'a) Fold.colmonoid;
  resource: ('a,'s,unit) Odist_stream.sink Odist_stream.resource;
}

(* Streams the action of items of a collection over a statefull and/or effectfull system.

   [col |> stream action]
*)
val stream : ('a,'m,'s) action -> 'a Fold.col -> unit
val sstream : ('a,'m,'s) action -> 'a Odist_stream.src -> unit

(* Printing action of a stream of strings on standard output. *)
val to_printer : (string, Buffer.t, unit) action

(* Printing action of a stream of strings into a file. *)
val to_file_printer : string -> (string, Buffer.t, unit) action


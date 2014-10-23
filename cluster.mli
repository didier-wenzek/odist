open Action
open Fold

(** Type of cluster handlers. *)
type t

(** Type of outfan handlers. *)
type outfan

(**
  Open an output channel which scatters (int,string) pair over the cluster.
  The message of a pair (node_id, msg) is sent to the node with the given id.

  [scatter cluster channel_name]
  returns an action to be used to scatter all values of a collection over the channel.

  In practice this action, has to be adapted to encode values and to allot them to nodes of the cluster.
  [my_collection |> stream_to (scatter cluster channel_name |> encoding_with encode |> allotting_with hash)]
*)
val scatter: t -> string -> (int*string,outfan,unit) action

val encoding_with: ('a -> string) -> (int*string,outfan,unit) action -> (int*'a,outfan,unit) action

val allotting_with: ('a -> int) -> (int*'a,outfan,unit) action -> ('a,outfan,unit) action

val using_roundrobin: (int*'a,outfan,unit) action -> ('a,int*outfan,unit) action

(**
  [gather cluster channel_name] gathers (node,message) of type (int,string) received from the cluster on the given channel.
  The gathering ends when all nodes of the cluster has called the [term ()] method of scattering action.
*)
val gather: t -> string -> (int * string) Fold.col

val ignore_order: (int * string) Fold.col -> string Fold.col

(** [col |> distribute node_count] distributes the elements of a collection over the cluster nodes.*)
val distribute: int -> 'a Fold.col -> 'a Fold.col

(** [using_context init term f arg] applies the function [f]
    to the [context] value returned by [init arg],
    ensures that [term context] is called
    and returns the value of [f context].
*)
val using_context : ('arg -> 'ctx) -> ('ctx -> 'unit) -> ('ctx -> 'res) -> 'arg -> 'res

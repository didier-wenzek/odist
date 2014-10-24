(** Given a cluster [t], [t.distribute col] wraps the collection [col] into a new collection which computations are distributed over the cluster nodes.*)
type cluster = {
  distribute: 'a. 'a Fold.col -> 'a Fold.col
}

val mcores : ?ipc_dir:string -> int -> cluster


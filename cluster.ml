open Action
open Fold
open Infix
open Col
open Red
open Util

module NodeSet = Set.Make(struct
  type t = int
  let compare = compare
end)

let fork task arg =
  match Unix.fork() with
  | 0 -> (try task arg; exit 0 with error -> exit 1)
  | _ -> Sys.set_signal Sys.sigchld Sys.Signal_ignore

let fork_n n task =
  for node_id = 0 to (n-1) do
    match Unix.fork() with
    | 0 -> (try task node_id; exit 0 with error -> exit 1)
    | _ -> Sys.set_signal Sys.sigchld Sys.Signal_ignore
  done

let marshall_encoding = ((fun a -> Marshal.to_string a []), (fun s -> Marshal.from_string s 0))

let make_reducer empty append merge =
  {
    empty = empty;
    append = append;
    merge = merge;
    result = id;
    absorber = None;
  }

module ZmqPullPushCores : sig
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

  (** [col |> distribute ipcdir node_count] distributes the elements of a collection over the cluster nodes.*)
  val distribute: string -> int -> 'a Fold.col -> 'a Fold.col

end = struct
  type t = {
    context: Zmq.context;
    ipcdir: string;
    size: int;
    this_node: string;
  }
  
  type outfan = Zmq.socket array
  
  let node_set cluster = range 0 (cluster.size - 1) |> reduce (to_set (module NodeSet))
  
  let init ipcdir size this_node = {
     context = Zmq.ctx_new ();
     size = size;
     ipcdir = ipcdir;
     this_node = string_of_int this_node;
  }
  
  let term cluster = Zmq.ctx_destroy cluster.context
  
  let local_channel ipcdir channel_name point = Printf.sprintf "ipc:///%s/%s.%s" ipcdir channel_name point
  
  let connect_outfan cluster channel_name point =
    let channel = local_channel cluster.ipcdir channel_name (string_of_int point) in
    let outfan = Zmq.socket cluster.context Zmq.PUSH in
    Zmq.connect outfan channel;
    outfan
  
  let bind_infan cluster channel_name =
    let channel = local_channel cluster.ipcdir channel_name cluster.this_node in
    let infan = Zmq.socket cluster.context Zmq.PULL in 
    Zmq.bind infan channel;
    infan
  
  let send this_node msg outfan =
    Zmq.send_multiparts outfan ["M"; this_node; msg]
    
  let close this_node outfan =
    Zmq.send_multiparts outfan ["E"; this_node]; Zmq.close outfan
  
  let scatter cluster channel_name  =
    let connect_outfan = connect_outfan cluster channel_name in
    let close_outfan = close cluster.this_node in
    let connect () = Array.init cluster.size connect_outfan in
    let push (node,msg) outfans = send cluster.this_node msg outfans.(node mod cluster.size);outfans in
    {
       init = connect;
       act = push;
       term = Array.iter close_outfan;
    }
  
  let gather_fold cluster channel_name append acc =
    let nodes = node_set cluster in
    let infan = bind_infan cluster channel_name in
    let rec loop nodes acc = 
      let msg = Zmq.receive_multiparts infan in
      match msg with
      | ["M";node;data] -> (
        let node = int_of_string node in
        let acc = append acc (node,data) in
        loop nodes acc
      )
      | ["E";node] -> (
        let node = int_of_string node in
        let nodes = NodeSet.remove node nodes in
        if NodeSet.is_empty nodes
        then (Zmq.close infan; acc)
        else loop nodes acc
      )
      | _ -> loop nodes acc
    in try loop nodes acc
       with error -> Zmq.close infan; raise error
  
  let gather cluster channel_name =
    let fold append _ acc = gather_fold cluster channel_name append acc in
    {
      fold = fold;
    }

  let encoding_with encode action =
    let encode_than_act (nodeid,item) = action.act (nodeid,encode item) in
    {
       action with
       act = encode_than_act;
    }
  
  let allotting_with hash action =
    let allot_than_act item = action.act (hash item,item) in
    {
       action with
       act = allot_than_act;
    }
  
  let using_roundrobin action =
    let allot_than_act item = (fun (lot,state) -> (lot+1, action.act (lot,item) state)) in
    {
       init = (fun () -> (0,action.init ()));
       act = allot_than_act;
       term = (fun (_,state) -> action.term state);
    }
  
  let ignore_order node_msg_pairs =
    let fold append = node_msg_pairs.fold (fun acc (_,msg) -> append acc msg) in
    {
       fold = fold
    }
  
  let distribute ipcdir size col =
    let using_cluster m task = using_context (init ipcdir m) term task in
    let launch_bg n m task = fork_n n (task |> using_cluster m) in
    let launch_fg   m task = using_cluster m task 0 in
    let encode,decode = marshall_encoding in 
    let stream_to_channel cluster name = stream_to (scatter cluster name |> encoding_with encode |> using_roundrobin) in
    let gather_from_channel cluster name = gather cluster name |> ignore_order |> map decode in
    let par_fold append merge seed =
       let partial_reducer = make_reducer seed append merge in
       let final_reducer = make_reducer seed merge merge in
       begin
         launch_bg 1 size (fun cluster -> col |> stream_to_channel cluster "AAA");
         launch_bg size 1 (fun cluster -> gather_from_channel cluster "AAA" |> reduce partial_reducer |> single |> stream_to_channel cluster "BBB");
         launch_fg size   (fun cluster -> gather_from_channel cluster "BBB" |> reduce final_reducer)
       end
    in
    {
       fold = par_fold
    }
end
 
type cluster = {
  distribute: 'a. 'a Fold.col -> 'a Fold.col
}

let mcores ?(ipc_dir = "/tmp") core_count = {
    distribute = (fun col -> ZmqPullPushCores.distribute ipc_dir core_count col);
  }


open Action
open Fold
open Infix
open Col
open Red
open Util

module NodeSet = MakeSetRed(struct
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

let make_channel_name =
  let pid = Unix.getpid () in
  let seq = ref 0 in
  let next_channel_name ipc_dir =
      seq := !seq + 1;
      Printf.sprintf "%s/odist-%d-%d-channel" ipc_dir pid !seq
  in next_channel_name

let ipc_channel channel_name point = Printf.sprintf "ipc:///%s.%s" channel_name point

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
    [my_collection |> stream_to (scatter cluster channel_name |> allotting_with hash)]
  *)
  val scatter: t -> string -> (int*string, (int*string) list,outfan) action
  
  val allotting_with: ('a -> int) -> (int*'a,'b,outfan) action -> ('a,'b,outfan) action
  
  val using_roundrobin: (int*'a,'b,outfan) action -> ('a,'a list,int*outfan) action
  
  (**
    [gather cluster channel_name] gathers (node,message) of type (int,string) received from the cluster on the given channel.
    The gathering ends when all nodes of the cluster has called the [term ()] method of scattering action.
  *)
  val gather: t -> string -> (int * string) Fold.sfoldable
  
  val ignore_order: (int * string) Fold.sfoldable -> string Fold.sfoldable

  (** [col |> distribute ipcdir node_count] distributes the elements of a collection over the cluster nodes.*)
  val distribute: string -> int -> 'a Fold.col -> 'a Fold.col
  val fair_distribute: string -> int -> 'a Fold.col -> 'a Fold.col

end = struct
  type t = {
    context: Zmq.context;
    size: int;
    this_node: string;
  }
  
  type outfan = Zmq.socket array

  let node_set cluster = Col.of_range 0 (cluster.size - 1) |> reduce NodeSet.union_reducer
  
  let init size this_node = {
     context = Zmq.ctx_new ();
     size = size;
     this_node = string_of_int this_node;
  }
  
  let term cluster = Zmq.ctx_destroy cluster.context
  
  let connect_outfan cluster channel_name point =
    let channel = ipc_channel channel_name (string_of_int point) in
    let outfan = Zmq.socket cluster.context Zmq.PUSH in
    Zmq.connect outfan channel;
    outfan
  
  let bind_infan cluster channel_name =
    let channel = ipc_channel channel_name cluster.this_node in
    let infan = Zmq.socket cluster.context Zmq.PULL in 
    Zmq.bind infan channel;
    infan
  
  let send this_node msg outfan =
    Zmq.send_multiparts outfan ["M"; this_node; msg]
    
  let close this_node outfan =
    Zmq.send_multiparts outfan ["E"; this_node]; Zmq.close outfan
  
  let revlist_reducer = {
         empty = (fun () -> []);
         append = (fun xs x -> x::xs); 
         merge = (fun xs ys -> ys @ xs);
         result = id;
         maximum = None; 
       }
  let fold_revlist push = (fun acc xs -> let xs = List.rev xs in List.fold_left push acc xs)

  let scatter cluster channel_name  =
    let connect_outfan = connect_outfan cluster channel_name in
    let close_outfan = close cluster.this_node in
    let connect () = Array.init cluster.size connect_outfan in
    let push outfans (node,msg) = send cluster.this_node msg outfans.(node mod cluster.size);outfans in
    {
       reducer = revlist_reducer;
       init = connect;
       push_item = push;
       push = fold_revlist push;
       term = Array.iter close_outfan;
    }
  
  let connect_outfan cluster channel_name point =
    let channel = ipc_channel channel_name (string_of_int point) in
    let outfan = Zmq.socket cluster.context Zmq.PUSH in
    Zmq.connect outfan channel;
    outfan

  let fair_scatter cluster channel_name  =
    let connect_outfan outfan point =
      let channel = ipc_channel channel_name (string_of_int point) in 
      Zmq.connect outfan channel; outfan
    in
    let to_outfan_connector = {
      empty = (fun () -> Zmq.socket cluster.context Zmq.PUSH);
      append = connect_outfan;
      merge = (fun a b -> a); (* FIXME *)
      result = id;
      maximum = None;
    } in
    let push outfan msg = send cluster.this_node msg outfan;outfan in
    {
       reducer = revlist_reducer;
       init = (fun () -> Col.of_range 0 (cluster.size -1) |> reduce to_outfan_connector);
       push_item = push;
       push = fold_revlist push;
       term = close cluster.this_node;
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
    let fold red acc = gather_fold cluster channel_name red acc in
    {
      sfold = fold;
    }

  let allotting_with hash action =
    let pair_with_hash item = (hash item,item) in
    let allot_than_act s item = action.push_item s (hash item,item) in
    {
       action with
       reducer = mapping pair_with_hash action.reducer;
       push_item = allot_than_act;
    }
  
  let using_roundrobin action =
    let allot_than_act (lot,state) item = (lot+1, action.push_item state (lot,item)) in
    {
       reducer = revlist_reducer;
       init = (fun () -> (0,action.init ()));
       push_item = allot_than_act;
       push = fold_revlist allot_than_act;
       term = (fun (_,state) -> action.term state);
    }

  let ignore_order node_msg_pairs =
    let ignoring_order red = (fun acc (_,msg) -> red acc msg); in
    let fold red = node_msg_pairs.sfold (ignoring_order red) in
    {
       sfold = fold
    }
  
  let distribute ipcdir size col =
    let using_cluster m task = using_context (init m) term task in
    let launch_bg n m task = fork_n n (task |> using_cluster m) in
    let launch_fg   m task = using_cluster m task 0 in
    let encode,decode = marshall_encoding in 
    let gather_from_channel cluster name = gather cluster name |> ignore_order |> smap decode in
    let stream_to_channel cluster name = smap encode >> sstream (scatter cluster name |> using_roundrobin) in
    let par_fold part_reducer comb_reducer seed =
       let channel_A = make_channel_name ipcdir in
       let channel_B = make_channel_name ipcdir in
       begin
         launch_bg 1 size (fun cluster -> col |> to_sfoldable |> stream_to_channel cluster channel_A);
         launch_bg size 1 (fun cluster -> gather_from_channel cluster channel_A |> fun xs -> Stream xs |> part_reducer |> stream_to_channel cluster channel_B);
         launch_fg size   (fun cluster -> gather_from_channel cluster channel_B |> sfold comb_reducer seed)
       end
    in
    Parcol {
       pfold = par_fold
    }

  let fair_distribute ipcdir size col =
    let using_cluster m task = using_context (init m) term task in
    let launch_bg n m task = fork_n n (task |> using_cluster m) in
    let launch_fg   m task = using_cluster m task 0 in
    let encode,decode = marshall_encoding in 
    let stream_to_channel cluster name = smap encode >> sstream (fair_scatter cluster name) in
    let gather_from_channel cluster name = gather cluster name |> ignore_order |> smap decode in
    let par_fold part_reducer comb_reducer seed =
       let channel_A = make_channel_name ipcdir in
       let channel_B = make_channel_name ipcdir in
       begin
         launch_bg 1 size (fun cluster -> col |> to_sfoldable |> stream_to_channel cluster channel_A);
         launch_bg size 1 (fun cluster -> gather_from_channel cluster channel_A |> fun xs -> Stream xs |> part_reducer |> stream_to_channel cluster channel_B);
         launch_fg size   (fun cluster -> gather_from_channel cluster channel_B |> sfold comb_reducer seed)
       end
    in
    Parcol {
       pfold = par_fold
    }
end
 
type cluster = {
  distribute: 'a. 'a Fold.col -> 'a Fold.col
}

let mcores ?ipc_dir core_count =
  let ipc_dir = match ipc_dir with
    | None -> Filename.get_temp_dir_name ()
    | Some(ipc_dir) -> ipc_dir
  in
  {
    distribute = (fun col -> ZmqPullPushCores.distribute ipc_dir core_count col);
  }


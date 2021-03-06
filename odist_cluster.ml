module Fold = Odist_fold
module Stream = Odist_stream
module Red = Odist_red
module Col = Odist_col
open Odist_fold
open Odist_util
open Odist_infix

module NodeSet = Red.MakeSetRed(struct
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
    returns a sink to be used to scatter all values of a collection over the channel.
  
    In practice this sink, has to be adapted to encode values and to allot them to nodes of the cluster.
    [my_collection |> allot_with hash |> stream (scatter cluster channel_name)]
  *)
  val scatter: t -> string -> (int*string,outfan,unit) Stream.sink Stream.resource
  
  val allot_with: ('a -> int) -> 'a Stream.src -> (int*'a) Stream.src
  val roundrobin: string Stream.src -> (int*string) Stream.src
  
  (**
    [gather cluster channel_name] gathers (node,message) of type (int,string) received from the cluster on the given channel.
    The gathering process ends when all nodes of the cluster has called the [term ()] method of they scattering sink.
  *)
  val gather: t -> string -> (int * string) Stream.src
  
  val ignore_order: (int * string) Stream.src -> string Stream.src

  (** [col |> distribute ipcdir node_count] distributes the elements of a collection over the cluster nodes.*)
  val distribute: string -> int -> 'a col -> 'a col
  val fair_distribute: string -> int -> 'a col -> 'a col

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
  
  let scatter cluster channel_name  =
    let connect_outfan = connect_outfan cluster channel_name in
    let close_outfan = close cluster.this_node in
    let connect () = Array.init cluster.size connect_outfan in
    let push outfans (node,msg) = send cluster.this_node msg outfans.(node mod cluster.size);outfans in
    let close outfans = fun () -> Array.iter close_outfan outfans in
    let sink outfans =
    {
      Stream.init = (fun () -> outfans); 
      Stream.push = push;
      Stream.term = ignore;
      Stream.full = None
    } in
    (fun () -> let hdl = connect () in (sink hdl, close hdl))
  
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
    let connect () = Col.of_range 0 (cluster.size -1) |> fold connect_outfan (Zmq.socket cluster.context Zmq.PUSH) in
    let close outfans () = close cluster.this_node outfans in
    let push outfan msg = send cluster.this_node msg outfan;outfan in
    let sink outfans =
    {
      Stream.init = (fun () -> outfans);
      Stream.push = push;
      Stream.term = ignore;
      Stream.full = None
    } in
    (fun () -> let hdl = connect () in (sink hdl, close hdl))
  
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
    Stream.Stream {
      Stream.sfold = fold;
    }

  let allotting_with hash sink =
    Stream.{
      sink with
      push = (fun hdl item -> sink.push hdl (hash item,item));
    }

  let allot_with hash msgs =
    let adapt sink = allotting_with hash sink in
    Stream.Transf {
       Stream.tfold = (fun sink -> msgs |> Stream.stream (adapt sink))
    }
  
  let using_roundrobin sink =
    Stream.{
      init = (fun () -> (0,sink.init ())); 
      push = (fun (lot,hdl) item -> (lot+1, sink.push hdl (lot,item)));
      term = (fun (_,hdl) -> sink.term hdl);
      full = match sink.full with
        | None -> None
        | Some(maximum) -> Some (fun (_,hdl) -> maximum hdl);
    }

  let roundrobin msgs =
    Stream.Transf {
       Stream.tfold = (fun sink -> msgs |> Stream.stream (using_roundrobin sink))
    }

  let ignoring_order sink =
    Stream.{
      sink with
      push = (fun hdl (_,msg) -> sink.Stream.push hdl msg)
    }

  let ignore_order node_msg_pairs =
    Stream.Transf {
       Stream.tfold = (fun sink -> node_msg_pairs |> Stream.stream (ignoring_order sink))
    }
  
  let distribute ipcdir size col =
    let using_cluster m task = using_context (init m) term task in
    let launch_bg n m task = fork_n n (task |> using_cluster m) in
    let launch_fg   m task = using_cluster m task 0 in
    let encode,decode = marshall_encoding in 
    let gather_from_channel cluster name = gather cluster name |> ignore_order |> Stream.map decode in
    let stream_to_channel cluster name = Stream.map encode >> roundrobin >> Stream.stream_to (scatter cluster name) in
    let par_fold part_reducer comb_reducer seed =
       let channel_A = make_channel_name ipcdir in
       let channel_B = make_channel_name ipcdir in
       begin
         launch_bg 1 size (fun cluster -> col |> to_stream |> stream_to_channel cluster channel_A);
         launch_bg size 1 (fun cluster -> gather_from_channel cluster channel_A |> part_reducer |> stream_to_channel cluster channel_B);
         launch_fg size   (fun cluster -> gather_from_channel cluster channel_B |> Stream.fold comb_reducer seed)
       end
    in
    Parcol {
       pfold = (fun part_reducer -> Stream.Stream { Stream.sfold = (fun comb_reducer -> par_fold part_reducer comb_reducer) })
    }

  let fair_distribute ipcdir size col =
    let using_cluster m task = using_context (init m) term task in
    let launch_bg n m task = fork_n n (task |> using_cluster m) in
    let launch_fg   m task = using_cluster m task 0 in
    let encode,decode = marshall_encoding in 
    let stream_to_channel cluster name = Stream.map encode >> Stream.stream_to (fair_scatter cluster name) in
    let gather_from_channel cluster name = gather cluster name |> ignore_order |> Stream.map decode in
    let par_fold part_reducer comb_reducer seed =
       let channel_A = make_channel_name ipcdir in
       let channel_B = make_channel_name ipcdir in
       begin
         launch_bg 1 size (fun cluster -> col |> to_stream |> stream_to_channel cluster channel_A);
         launch_bg size 1 (fun cluster -> gather_from_channel cluster channel_A |> part_reducer |> stream_to_channel cluster channel_B);
         launch_fg size   (fun cluster -> gather_from_channel cluster channel_B |> Stream.fold comb_reducer seed)
       end
    in
    Parcol {
       pfold = (fun part_reducer -> Stream.Stream { Stream.sfold = (fun comb_reducer -> par_fold part_reducer comb_reducer) })
    }
end
 
type cluster = {
  distribute: 'a. 'a col -> 'a col
}

let mcores ?ipc_dir core_count =
  let ipc_dir = match ipc_dir with
    | None -> Filename.get_temp_dir_name ()
    | Some(ipc_dir) -> ipc_dir
  in
  {
    distribute = (fun col -> ZmqPullPushCores.distribute ipc_dir core_count col);
  }


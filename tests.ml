open Odist
open Infix

let even n = n mod 2 == 0
let square n = n * n
let sum = monoid 0 (+)
let sum_square_of_evens = filter even >> map square >> reduce sum

type 'a nested_list = L of 'a list | N of 'a nested_list list
let fold_nested_list append merge empty =
    let rec fold l = match l with
    | L xs -> List.fold_left append empty xs
    | N xxs -> List.fold_left (fun a xs -> merge a (fold xs)) empty xxs
    in fold

let nested_list xs =
    let foldxs append merge empty = fold_nested_list append merge empty xs in
    {
      fold = foldxs
    }

module S = MakeSetRed(struct
  type t = int
  let compare = compare
end)

let _ =
  let s = range 1 100 |> filter even |> map square |> reduce sum in
  assert( s = 171700);

  let s = range 1 100 |> sum_square_of_evens in
  assert( s = 171700);

  let s = list [1;2;3;4;5] |> sum_square_of_evens in
  assert( s = 20);

  let s = nested_list (N [L [1;2;3]; L[4;5;6;7]; N [ L[]; L[8;9] ]]) |> reduce to_list in
  assert (s = [1; 2; 3; 4; 5; 6; 7; 8; 9]);

  let fact n = (range 1 n |> reduce Int.product) in
  assert( 120 = (fact 5));

  let f_image f xs = list (xs) |> map f |> reduce S.union_reducer |> S.elements in
  assert(f_image square [-2; -1; 0; 1; 2 ] = [0; 1; 4]);

  let fs = files(".") in
  let fs_list = fs |> reduce to_list in
  let fs_count = fs |> reduce Int.count in
  assert( fs_count = List.length fs_list);

  let s = range 1 1000000000 |> exists even in
  assert (s);

  let s = range 1 1000000000 |> forall even in
  assert (not s);

  let s = range 1 1000000000 |> reduce first in
  assert (s = Some(1));

  let s = range 1 1000000000 |> reduce (sum |> taking 5) in
  assert (s = 15);

  let sumf = monoid 0.0 (+.) in
  let count = sumf |> mapping (fun _ -> 1.0) in
  let mean = pair_reducer sumf count |> returning (fun (total,n) -> if n = 0.0 then 0.0 else total /. n) in
  let m = list [1.2; 2.4; 3.6] |> reduce mean in
  assert (m = 2.4);

  let s = range 0 9 |> map string_of_int |> stream_to (string_buffer 16) in
  assert ( s = "0123456789");

  range 1 100 |> map string_of_int |> flatmap (fun s -> list [s;"\n"]) |> stream_to (file_printer "/tmp/foo");
  let s = lines "/tmp/foo" |> map int_of_string |> reduce sum in
  assert (s = 50 * 101 );

  let cores = Cluster.mcores 4 in
  let s_par = range 1 100 |> cores.distribute |> sum_square_of_evens in
  let s_seq = range 1 100 |>                     sum_square_of_evens in
  assert (s_par = s_seq);

  let chunk m i = range (m*i+1) (m*(i+1)) in
  let par_range n m = range 0 (n-1) |> cores.distribute |> flatmap (chunk m) in
  let seq_range n m = range 1 (n*m) in
  let s_par = par_range 4 25 |> sum_square_of_evens in
  let s_seq = seq_range 4 25 |> sum_square_of_evens in
  assert (s_par = s_seq);


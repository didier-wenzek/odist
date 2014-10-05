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

  let fs = files(".") in
  let fs_list = fs |> reduce to_list in
  let fs_count = fs |> reduce Int.count in
  assert( fs_count = List.length fs_list);

  let s = range 1 1000000000 |> exists even in
  assert (s);

  let s = range 1 1000000000 |> forall even in
  assert (not s);

  let s = range 0 9 |> map string_of_int |> stream_to (string_buffer 16) in
  assert ( s = "0123456789");

  range 1 100 |> map string_of_int |> flatmap (fun s -> list [s;"\n"]) |> stream_to (file_printer "/tmp/foo");
  let s = lines "/tmp/foo" |> map int_of_string |> reduce sum in
  assert (s = 50 * 101 );



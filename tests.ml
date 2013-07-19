open Odist
open Infix

let _ =
  let fact n = (range 1 n |> reduce Int.product) in
  assert( 120 = (fact 5));

  let fs = files(".") in
  let fs_list = fs |> reduce to_list in
  let fs_count = fs |> reduce Int.count in
  assert( fs_count = List.length fs_list);

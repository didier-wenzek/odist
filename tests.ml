open Odist
open Infix
open Red.Int

let _ =
  assert( 120 = (range 1 5 |> reduce product))


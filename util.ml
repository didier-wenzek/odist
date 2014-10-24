(** [using_context init term f arg] applies the function [f]
    to the [context] value returned by [init arg],
    ensures that [term context] is called
    and returns the value of [f context].

    [val using_context : ('arg -> 'ctx) -> ('ctx -> 'unit) -> ('ctx -> 'res) -> 'arg -> 'res]
*)
let using_context init term task arg =
  let ctx = init arg in
  try let result = task ctx in term ctx; result
  with error -> term ctx; raise error
  

(* Function that lets you return early from a computation.
   Adapted from Alan Frish's version of https://ocaml.janestreet.com/?q=node/91,
   with the constraint that the return function is only used
   in contexts having the same type as the whole expression.

   let sum_until_first_negative list =
     with_return (fun return ->
       List.fold_left (fun acc x -> if x >= 0 then acc + x else return acc)
                      0
                      list
     )
*)
let with_return (type t) f =
  let module M = struct exception Return of t end in
  try f (fun x -> raise (M.Return x)) with M.Return x -> x

(* Compute f x and print elapsed time. *)
let time f x =
  let t0 = Unix.gettimeofday () in
  let r = f x in
  let t1 = Unix.gettimeofday () in
  let d = (t1 -. t0) *. 1000.0 in
  Printf.printf "elapsed time: %.3f ms\n%!" d;
  r


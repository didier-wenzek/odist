OCaml distributed data processing
=================================

The core of ODist is dataset processing :

    open Odist
    open Infix

    let even n = n mod 2 == 0
    let square n = n * n
    let sum = monoid 0 (+)

    range 1 100 |> filter even |> map square |> reduce sum

All input datasets are abstracted by a single type `'a Odist.col`
which values can be uniformaly manipulated
whatever the underlying data structure.

    range 1 100
    list [1;2;3;4;5]
    lines "/tmp/foo"

    let sum_square_of_evens = filter even >> map square >> reduce sum

    range 1 100 |> sum_square_of_evens
    list [1;2;3;4;5] |> sum_square_of_evens
    lines "/tmp/foo" |> map int_of_string |> sum_square_of_evens

A dataset is only defined indirectly by its ability to fold its content using
- an empty initial aggregate,
- a function to inject one item into an aggregate,
- a function to merge two aggregates.

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

    nested_list (N [L [1;2;3]; L[4;5;6;7]; N [ L[]; L[8;9] ]]) |> reduce to_list

Then come dataset manipulators which take a dataset and transform it, removing, changing, adding items.
The result is a dataset, so transformations can be chained.

    map: ('a -> 'b) -> 'a Odist.col -> 'b Odist.col
    filter: ('a -> bool) -> 'a Odist.col -> 'a Odist.col
    flatmap: ('a -> 'b Odist.col) -> 'a Odist.col -> 'b Odist.col
    
    files "." |> flatmap words |> map String.lowercase |> filter (fun w -> (String.get w 0) = 'a')

Note that these transformations are lazy: they are performed only when the dataset is actually reduced into some aggregate.
Such a reduction is driven by a reducer which gives the three arguments expected by a collection to be folded plus a fourth one aimed to finalize the reduction:
- an `empty` initial aggregate,
- an `append` function aimed to inject one item into an aggregate,
- a `merge` function aimed to merge two aggregates,
- a `result` function which transform an aggregate into a final outcome.

This gives a reducer of type `(item, aggregate, outcome) Fold.red`, which reduces a collection of `item` into an `aggregate` and than into a final `outcome`.

Reducers are built around an associative binary operation with an identity element (*a.k.a. a monoid*).

    let sum = monoid 0 (+)
    let product = monoid 1 ( * )

    range 1 10 |> reduce sum
    range 1 10 |> reduce product

Reducers can be combined too:

    let count = red_map (fun _ -> 1) sum
    let mean = red_product Float.sum Float.count (/.)

    lines "/tmp/bar" |> map float_of_string |> reduce mean

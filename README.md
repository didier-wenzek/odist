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
whatever their underlying data structure.

    range 1 100
    list [1;2;3;4;5]
    lines "/tmp/foo"

    let sum_square_of_evens = filter even >> map square >> reduce sum

    range 1 100 |> sum_square_of_evens
    list [1;2;3;4;5] |> sum_square_of_evens
    lines "/tmp/foo" |> map int_of_string |> sum_square_of_evens

Note the `list [1;2;3;4;5]` construct which wraps a regular OCaml list into an abstract `Odist.col`-lection;
and, similarly, the `lines "/tmp/foo"` call which turns a file into a collection of strings.

Underneath, a collection is only indirectly defined by its ability to fold its content using
- a function to inject one item into an aggregate,
- a function to merge two aggregates,
- an initial aggregate.


    type 'a col = {
      fold: 'b. ('b -> 'a -> 'b) -> ('b -> 'b -> 'b) -> 'b  -> 'b;
    }

For instance, we can implement a collection using a tree of nested lists as representation:

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

Note that these transformations are lazy: they are only performed when the dataset is actually reduced into some aggregate. For instance, underneath a mapped collection simply waits for a call to the `fold` function to transform the given `append` argument and to forward the call to the former collection.

    let map f col =
      let transform append = (fun acc item -> append acc (f item)) in
      {
        fold = (fun append merge seed -> col.fold (transform append) merge seed);
      }


Reductions are driven by reducers which provide the arguments by a collection to be folded.
A reducer of type `(item, aggregate, outcome) Fold.red` reduces a collection of `item` into an `aggregate` and than into a final `outcome`. It provides:
- an `empty` initial aggregate,
- an `append` function aimed to inject one item into an aggregate,
- a `merge` function aimed to merge two aggregates,
- a `result` function which transform an aggregate into a final outcome.

Reducers are built around an associative binary operation with an identity element (*a.k.a. a monoid*).

    let sum = monoid 0 (+)
    let product = monoid 1 ( * )

    range 1 10 |> reduce sum
    range 1 10 |> reduce product

Reducers can be combined too:

    let count = red_map (fun _ -> 1) sum

    let sum_float = monoid 0.0 (+.)
    let mean = red_product sum_float count (fun s c -> s /. (float_of_int c))

    lines "/tmp/bar" |> map float_of_string |> reduce mean

Note that the type of `mean` is `(float, float * int, float) Odist.red`: the accumulated values are `(sum, count)` pairs which can only be translated into a result when the reduction is fully done. Hence the `result` function of any reducer. In the `mean` reducer case, the `result` of a final `(sum, count)` pair is simply the `sum` divided by the `count`.

Another way a collection can be reduced is by streaming its content to some effectfull device.

    list ["foo";"bar"] |> stream_to (file_printer "/tmp/foo")

`file_printer "/tmp/foo"` defines an action of type `(string, out_channel, unit) Odist.action` *i.e.* an action
which threads string action to an `out_channel` and finally returns a `unit` result. Such an action is 3 parts :

    type ('a,'s,'b) action = {
      init: unit -> 's;
      act: 'a -> 's -> 's;
      term: 's -> 'b;
    }

A call to `[col |> stream_to action]`, 
- starts to initialize the statefull system calling [action.init] getting here an `out_channel`.
- Then each items of the collection [col] is used to act on the system,
  applying [action.act item] on the current state to get the new one.
  Here each string is printed to the channel.
- Finally the system resources are released using [action.term],
  closing the channel in the case of `file_printer`.
    



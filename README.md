OCaml distributed data processing
=================================

The core of ODist is dataset processing :

    open Odist
    open Infix

    let even n = n mod 2 == 0
    let square n = n * n
    let sum = monoid 0 (+)

    range 1 100 |> filter even |> map square |> reduce sum

The aim of ODist is to abstract data processing in such a way
that processing a distributed dataset can be done
using the same processing pipeline as a local dataset.
For instance, counting the occurrences of each word in a list of files
can be defined as a `word_count` function which can be indifferently applied 
to local and distributed sets of files.

    module StrMap = MakeMapRed(String)
    let grouping = StrMap.grouping
    let count = sum |> mapping (fun x -> 1)
    
    let word_count = flatmap words >> reduce (grouping count)

    (* Count words of all files in the working directory. *)
    files "." |> word_count

    (* Use 4 process to count words. *)
    let cores = Cluster.mcores 4
    files "." |> cores.distribute |> word_count

For that, all input datasets are abstracted by a single type `'a Odist.col`
which values can be uniformaly manipulated
whatever is the underlying data structure.

    range 1 100
    list [1;2;3;4;5]
    lines "/tmp/foo"

    let sum_square_of_evens = filter even >> map square >> reduce sum

    range 1 100                           |> sum_square_of_evens
    range 1 100 |> cores.distribute       |> sum_square_of_evens
    list [1;2;3;4;5]                      |> sum_square_of_evens
    lines "/tmp/foo" |> map int_of_string |> sum_square_of_evens

Note the `list [1;2;3;4;5]` construct which wraps a regular OCaml list into an abstract `Odist.col`-lection.
Similarly, `lines "/tmp/foo"` turns a file into a collection of strings.

In a parallel or distributed setting (latter is not yet implemented),
a former collection is broken into parts, one per computing unit.
These parts are processed in parallel to produce result parts
which has to be reduced in a final outcome.
A key point is how the former dataset is distributed over computing units.

    (* Sequential computation *)
    (* 15.6 s on my desktop *)
    let seq_range n m = range 1 (n*m)
    seq_range 4 25000000 |> sum_square_of_evens

    (* Sequential construction of the dataset, parallel computation. *)
    (* 14.6 s on my desktop *)
    seq_range 4 25000000 |> cores.distribute |> sum_square_of_evens

    (* Parallel construction of the dataset and computation. *)
    (* 4.6 s on my desktop. *)
    let chunk m i = range (m*i+1) (m*(i+1))
    let par_range n m = range 0 (n-1) |> cores.distribute |> flatmap (chunk m)
    par_range 4 25 |> sum_square_of_evens

Underneath, a collection is only indirectly defined by its ability to fold its content using
- a function to inject one item into an aggregate,
- a function to merge two aggregates,
- an initial aggregate.

A collection of `'a` has type:
     
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
    
    let lowercase = String.lowercase
    let stopword w = Set.mem w stopwords
    files "." |> flatmap words |> map lowercase |> filter stopword

Note that these transformations are lazy: they are only performed when the dataset is actually reduced into some aggregate. For instance, underneath a mapped collection simply waits for a call to the `fold` function to transform the given `append` argument and to forward the call to the former collection.

    let map f col =
      let transform append = (fun acc item -> append acc (f item)) in
      {
        fold = (fun append merge seed -> col.fold (transform append) merge seed);
      }


Reductions are driven by reducers which provide the arguments required by a collection to be folded.
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

    let fsum = monoid 0.0 (+.)
    let fcount = fsum |> mapping (fun _ -> 1.0)
    let mean = pair_reducer fsum fcount |> returning (fun (total,n) -> if n = 0.0 then 0.0 else total /. n)

    list [1.2; 2.4; 3.6] |> reduce mean

Note that the type of `mean` reducer has type `(float, float * float, float) Odist.red`:
- it accumulates `float` values
- into a `(sum, count)` pair,
- which is finally transformed into a `float` result.

Early termination may be handled by reducers.
For that a `maximum` value or checking function is attached to the reducer,
so a reduce operation can be stop as soon as the accumulated value has reach a maximum
(i.e a value which will not be changed by further accumulations).

    let product = monoid 1 ( * ) |> with_maximum 0

    range 0 1000000 |> reduce product
    
    let taking n reducer =
      let max_count_reach (c,_) = c >= n in
      let reduced_head (_,xs) = list xs |> reduce reducer in
      pair_reducer count to_list |> with_maximum_check max_count_reach |> returning reduced_head

    range 0 1000000 |> reduce (to_list |> tacking 10)

Another way to reduce a collection is to stream its content to some effectfull device.

    list ["foo";"bar"] |> stream_to (file_printer "/tmp/foo")

`file_printer "/tmp/foo"` defines an action of type `(string, out_channel, unit) Odist.action` *i.e.* an action
which threads string action to an `out_channel` and finally returns a `unit` result. Such an action is 3 parts :

    type ('a,'s,'b) action = {
      init: unit -> 's;
      act: 'a -> 's -> 's;
      term: 's -> 'b;
    }

A call to `[col |> stream_to action]`, 
- starts to initialize the statefull system calling `action.init] getting here an `out_channel`.
- Then each items of the collection `col` is used to act on the system,
  applying `action.act item` on the current state to get the new one.
  Here each string is printed to the channel.
- Finally the system resources are released using `action.term`,
  closing the channel in the case of `file_printer`.
    



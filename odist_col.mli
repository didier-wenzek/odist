(** Collections *)

module Fold = Odist_fold

val empty: 'a Fold.col
(* The empty collection. *)

val single: 'a -> 'a Fold.col
(* [single i] wraps the item [i] into a foldable collection. *)

val append: 'a Fold.col -> 'a -> 'a Fold.col
(* [append xs x] appends [x] to the collection [xs]. *)

val concat: 'a Fold.col -> 'a Fold.col -> 'a Fold.col
(* [concat xs ys] concats the two collections into a new foldable collection. *)

val of_list: 'a list -> 'a Fold.col
(* [Col.of_list l] wraps the ocaml list [l] into a foldable collection. *)

val of_array: 'a array -> 'a Fold.col
(* [Col.of_array a] wraps the ocaml array [a] into a foldable collection. *)

val of_array_i: 'a array -> (int*'a) Fold.col
(* [Col.of_array_i a] wraps the ocaml array [a] into a foldable collection of ['a] values with their index in the array. *)

val of_range: int -> int -> int Fold.col
(* [Col.of_range min max] builds the foldable collection of all integers from [min] to [max] inclusive. *)

val of_subdirs: ?recursive:bool -> string -> string Fold.col
(* [Col.of_subdirs rootpath] returns recursively all sub-directories of the given directory (including itself).
   [Col.of_subdirs ~recursive:false rootpath] returns all direct sub-directories of the given directory (excluding itself).
*)

val of_files: ?recursive:bool -> string -> string Fold.col
(* [Col.of_files rootpath] returns recursively all regular files of the given directory.
   [Col.of_files ~recursive:false rootpath] returns recursively all direct regular files of the given directory.
*)

val of_file_chunks: int -> string -> string Fold.col
(* [Col.of_file_chunks size path] breaks the file in chunks of the given size. *)

val of_file_chars: string -> char Fold.col
(* [Col.of_file_chars path] returns all characters of the given file. *)

val of_file_lines: string -> string Fold.col
(* [Col.of_file_lines path] returns all lines of the given file. *)

val of_file_words: ?is_separator:(char->bool) -> string -> string Fold.col
(* [Col.of_file_words path] returns all words of the given file. *)

val of_string_words: ?is_separator:(char->bool) -> string -> string Fold.col
(* [Col.of_string_words s] returns all words of the given string. *)

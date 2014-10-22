(** Collections *)

val empty: 'a Fold.col
(* The empty collection. *)

val single: 'a -> 'a Fold.col
(* [single i] wraps the item [i] into a foldable collection. *)

val append: 'a Fold.col -> 'a -> 'a Fold.col
(* [append xs x] appends [x] to the collection [xs]. *)

val concat: 'a Fold.col -> 'a Fold.col -> 'a Fold.col
(* [concat xs ys] concats the two collections into a new foldable collection. *)

val list: 'a list -> 'a Fold.col
(* [list l] wraps the ocaml list [l] into a foldable collection. *)

val range: int -> int -> int Fold.col
(* [range min max] builds the foldable collection of all intergers from [min] to [max] inclusive. *)

val files: string -> string Fold.col
(* [files rootpath] returns recursively all files of the given directory. *)

val file_chunks: int -> string -> string Fold.col
(* [file_chunks size path] breaks the file in chunks of the given size. *)

val file: string -> char Fold.col
(* [file path] returns all characters of the given file. *)

val lines: string -> string Fold.col
(* [lines path] returns all lines of the given file. *)

val words: string -> string Fold.col
(* [words path] returns all words of the given file. *)

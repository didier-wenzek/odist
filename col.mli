(** Collections *)

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

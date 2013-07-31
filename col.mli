val list: 'a list -> 'a Fold.col
val range: int -> int -> int Fold.col

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

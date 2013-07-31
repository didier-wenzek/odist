type 'a split
val pack_split_reducer: (string -> string list) -> (string,'b,'c) Fold.red -> (string,'b split, 'c) Fold.red
(**
  [pack_split_reducer spliter reducer] builds a chunk list reducer from a string list reducer.

  The produced reducer combines 3 steps :
  - it appends all the chunks into a string,
  - it splits the string using the the regex separator,
  - it reduce the resulting string list using the former reducer.

  For instance to build a list of lines from a file;
  we may use a file reader which reads the file chunk by chunk using a fixed sized buffer. 
  Instead of packing these chunks into a whole string to be split again along end-of-line character,
  we transform the target reducer using the [pack_split_reducer] function
  which produce a reducer which directly works with the chunks provided by the file reader.

       let split_lines = Str.split_delim (Str.regexp "\n")
       let to_lines = pack_split_reducer split_lines to_list
       let file_lines = file_chunks 8000 >> reduce to_lines

*)


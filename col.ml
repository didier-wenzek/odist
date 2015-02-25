open Unix
open Fold

let empty = Stream {
    sfold = (fun _ e -> e);
  }

let single x = Stream {
    sfold = (fun red e -> red e x);
  }

let cons x xs = Stream {
   sfold = (fun red seed -> fold red (red seed x) xs );
}

let append xs x = Stream {
    sfold = (fun red e -> red (fold red e xs) x);
  }

let concat xs ys = Stream {
    sfold = (fun red e -> fold red (fold red e xs) ys);
  }

let of_list xs =
  let fold red acc = List.fold_left red acc xs in
  Stream {
    sfold = fold;
  }

let of_range min max =
  let fold red acc =
    let rec loop a i = if i>max then a else loop (red a i) (i+1)
    in loop acc min
  in
  Stream {
    sfold = fold;
  }

let of_array xs =
  let fold red acc = Array.fold_left red acc xs in
  Stream {
    sfold = fold;
  }

let of_array_i a =
  let fold red =
    let n = Array.length a in
    let get = Array.get a in
    let rec loop i s =
      if i = n then s
      else loop (i+1) (red s (i,get i))
    in loop 0 
  in
  Stream {
     sfold = fold
  }

let iterdir f path =
  let dir = opendir path in
  try
    while true do
      let name = readdir dir in
      if (name <> "." && name <> "..")
      then f (Filename.concat path name)
    done
  with End_of_file -> closedir dir
     | error -> closedir dir; raise error

(* [recfoldfiles comb seed directory] iterates recursively over all sub-paths of the given [directory]. *)
let recfoldfiles comb seed path =
  let acc = ref seed in
  let rec combfiles path =
    try
      match (lstat path).st_kind with
      | S_REG -> acc := comb !acc path
      | S_DIR -> iterdir combfiles path
      | _ -> ()
    with _ -> ()
  in combfiles path; !acc

(* [foldfiles comb seed directory] iterates over all direct sub-paths of the given [directory]. *)
let foldfiles comb seed path =
  let acc = ref seed in
  let combfiles path =
    try
      match (lstat path).st_kind with
      | S_REG -> acc := comb !acc path
      | _ -> ()
    with _ -> ()
  in let combfiles path =
    try
      match (lstat path).st_kind with
      | S_REG -> acc := comb !acc path
      | S_DIR -> iterdir combfiles path
      | _ -> ()
    with _ -> ()
  in combfiles path; !acc

let of_files ?(recursive = true) path = 
  if recursive
  then
    let fold red seed = recfoldfiles red seed path in
    Stream {
      sfold = fold
    }
  else
    let fold red seed = foldfiles red seed path in
    Stream {
      sfold = fold
    }

(* [recfoldsubdirs comb seed directory] iterates recursively over all sub-dirs of the given [directory]. *)
let recfoldsubdirs comb seed path =
  let acc = ref seed in
  let rec combfiles path =
    try
      match (lstat path).st_kind with
      | S_DIR -> acc := comb !acc path ; iterdir combfiles path
      | _ -> ()
    with _ -> ()
  in combfiles path; !acc

(* [foldsubdirs comb seed directory] iterates over all sub-dirs of the given [directory]. *)
let foldsubdirs comb seed path =
  let acc = ref seed in
  let combfiles path =
    try
      match (lstat path).st_kind with
      | S_DIR -> acc := comb !acc path
      | _ -> ()
    with _ -> ()
  in iterdir combfiles path; !acc

let of_subdirs ?(recursive = true) path = 
  if recursive
  then
    let fold red seed = recfoldsubdirs red seed path in
    Stream {
      sfold = fold
    }
  else
    let fold red seed = foldsubdirs red seed path in
    Stream {
      sfold = fold
    }

(* [fold_file_chunks size path comb seed] reads chunks of the given size and combines them. *)
let fold_file_chunks size path comb seed =
  let channel = open_in path in
  let buffer = Bytes.create size in
  let rec loop acc =
    let l = input channel buffer 0 size in
    if l = 0
    then (close_in channel; acc)
    else loop (comb acc (String.sub buffer 0 l))
  in try loop seed
     with  error -> close_in channel; raise error

let of_file_chunks size path =
  let fold red seed = fold_file_chunks size path red seed in
  Stream {
    sfold = fold
  }

(** folds chars of substring. *)
let string_chars str start len comb seed =
  let get pos = String.get str pos in
  let last = start + len in
  let rec loop pos acc =
    if pos < last
    then loop (pos + 1) (comb acc (get pos))
    else acc
  in loop start seed

(* [file_characters path comb seed] combines all characters of the file with the given [path]. *)
let file_characters path comb seed =
  let channel = open_in path in
  let buffer = Bytes.create 8192 in
  let rec loop acc =
    let l = input channel buffer 0 8192 in
    if l = 0
    then (close_in channel; acc)
    else loop (string_chars buffer 0 l comb acc)
  in try loop seed
     with  error -> close_in channel; raise error

let of_file_chars path =
  let fold red seed = file_characters path red seed in
  Stream {
    sfold = fold
  }

(* string processing *)
let token_combiner is_sep seed comb term =
  let newcomb s c =
    if is_sep c
    then match s with
         | (acc,Some(token)) -> (comb acc (Buffer.contents token), None)
         | _ -> s
    else match s with
         | (_,Some(token)) -> Buffer.add_char token c; s
         | (acc,None) -> let token = Buffer.create 16 in
                         Buffer.add_char token c;
                         (acc, Some(token))
  and newterm s = match s with
                  | (acc,None) -> term acc
                  | (acc,Some(token)) -> term (comb acc (Buffer.contents token))
  in ((seed,None), newcomb, newterm)

let isalpha c = 'A'<=c && c<='Z' || 'a'<=c && c<='z' || '0'<=c && c<='9'

let id x = x

let tokens is_sep file comb seed =
  let newseed,newcomb,newterm = token_combiner is_sep seed comb id in
  newterm (file_characters file newcomb newseed)

(* [words path comb seed] iterates over all words of the file with the given [path]. *)
let of_file_words path =
  let sep c = not (isalpha c) in
  let fold red = tokens sep path red in
  Stream {
    sfold = fold
  }

let of_file_lines path =
  let sep c = c = '\n' in
  let fold red seed = tokens sep path red seed in
  Stream {
    sfold = fold
  }

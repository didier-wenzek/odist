open Unix
open Fold

(* [foldfiles comb seed path] iterates recursively over all sub-paths of the directory with the given [path]. *)
let foldfiles comb seed path =
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
  in
  let acc = ref seed in
  let rec combfiles path =
    try
      match (lstat path).st_kind with
      | S_REG -> acc := comb !acc path
      | S_DIR -> iterdir combfiles path
      | _ -> ()
    with _ -> ()
  in combfiles path; !acc

let files path = 
  let fold append _ seed = foldfiles append seed path in
  {
    fold = fold
  }

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
  let buffer = String.create 81920 in
  let rec loop acc =
    let l = input channel buffer 0 81920 in
    if l = 0
    then (close_in channel; acc)
    else loop (string_chars buffer 0 l comb acc)
  in try loop seed
     with  error -> close_in channel; raise error

let file path =
  let fold append _ seed = file_characters path append seed in
  {
    fold = fold
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
let words path =
  let sep c = not (isalpha c) in
  let fold append _ = tokens sep path append in
  {
    fold = fold
  }

let lines path =
  let sep c = c = '\n' in
  let fold append _ seed = tokens sep path append seed in
  {
    fold = fold
  }

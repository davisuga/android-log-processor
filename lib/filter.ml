module String = struct
  let catStringWith separator strA strB = strA ^ separator ^ strB
  let joinArray separator arr = Array.fold_left (catStringWith separator) "" arr
  let joinList separator arr = List.fold_left (catStringWith separator) "" arr
  let replace input output = Str.global_replace (Str.regexp_string input) output
end

module File = struct
  let read filename callback =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true do
        let new_line = input_line chan in
        let _ = callback new_line in
        lines := new_line :: !lines
      done;
      !lines
    with End_of_file ->
      close_in chan;
      List.rev !lines
end

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let string_of_chars chars =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

open Printf

let removeMultipleWhitespaces str =
  let re = Str.regexp_string "\\s+" in
  Str.global_replace re " " str

let split_by_whitespaces = Str.split (Str.regexp "[ \t]+")

let print_list list =
  Stdlib.String.concat ", " list |> print_string |> print_newline

let parse_line line =
  let split_line = split_by_whitespaces line in
  let _ = print_list split_line in
  let _ =
    printf "before: %s,\n\n after: %s \n\n" line
      (Stdlib.String.concat "::" split_line)
  in
  match split_line with
  | date :: time :: pid :: pid2 :: level :: process :: msg ->
      String.joinList " " msg |> fun joinedMsg ->
      printf "date: %s time: %s pid: %s pid2: %s level: %s p: %s m: %s\n" date
        time pid pid2 level process joinedMsg
  | _ -> ()

let keepLinesWith substr = List.filter (fun str -> contains str substr)
let processFile fileName = File.read fileName parse_line

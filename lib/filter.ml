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

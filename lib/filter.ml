module String = struct
  let catStringWith separator strA strB = strA ^ separator ^ strB
  let joinArray separator arr = Array.fold_left (catStringWith separator) "" arr
  let joinList separator arr = List.fold_left (catStringWith separator) "" arr
  let replace input output = Str.global_replace (Str.regexp_string input) output
end

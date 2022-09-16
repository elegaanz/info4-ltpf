let rec min liste = match liste with
| [] -> failwith "Liste vide"
| [x] -> x
| x :: xs ->
  let min' = min xs in
  if min' < x then
    min'
  else
    x
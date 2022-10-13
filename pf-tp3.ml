let rec trouve_min_i l = function
| [] -> failwith "liste vide"
| x :: xs ->
    let (min, xs') = trouve_min_i xs in
    if x < min then
      (x, xs)
    else
      (min, x :: xs')

(* `cmp a b` renvoie true si a < b  *)
let rec trouve_min cmp l = function
| [] -> failwith "liste vide"
| x :: xs ->
    let (min, xs') = trouve_min_i xs in
    if cmp x min then
      (x, xs)
    else
      (min, x :: xs')

let trouve_min_i = trouve_min (<)

let tri_selection cmp l =
  let (min, l') = trouve_min cmp l in
  min :: (tri_selection cmp l')

let tri_selection_i = tri_selection (<)

let rec liste_alea = function
| 0 -> []
| x -> Random.int 1000 :: (liste_alea (x - 1))
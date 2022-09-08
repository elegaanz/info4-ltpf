let vaut5 = fun x -> x = 5
let vaut5_str = fun x -> if x = 5 then "oui" else "non"

(* Défintion récursive de la fonction factorielle *)

(* avec match *)
let rec fact x = match x with
| 0 -> 1
| _ -> x * (fact (x - 1))

(* avec function (match sur un argument qu'on a pas besoin de préciser) *)
let rec fact = function
| 0 -> 1
| x -> x * (fact (x - 1))

(* avec if, vu qu'on a que deux cas et des patterns simples *)
let rec fact x = if x = 0 then 1 else x * (fact (x - 1))

let calcul =
  let fact5 = fact 5 in
  let fact5double = 2 * fact5 in
  (2022 - fact5double) * fact5 + fact5double + fact5 - 1
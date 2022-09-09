open Ex5

(* ^ sert à écrire des puissances entières, pas à concaténer des strings *)
let rec (^) = fun a b -> if b = 0 then 1 else if b = 1 then a else a * (a ^ (b - 1))

let est_triplet_pythagoricien a b c = (a^2) + (b^2) = (c^2)

let meme_signe a b = Ex5.signe a = Ex5.signe b
(* L’aire d’un carré de côté a *)
let aire_carre a = a *. a
(* ou bien *)
let aire_carre = fun a -> a *. a

(* L’aire d’un rectangle de côtés a et b. *)
let aire_rectangle a b = a *. b
(* ou bien *)
let aire_rectangle = fun a b -> a *. b


(* L’aire d’un cercle de rayon r. *)
(* On commence par définir pi *)
let pi = 3.1415

let aire_cercle r = pi *. r *. r
(* ou *)
let aire_cercle = fun r -> pi *. r *. r

(* L’aire d’un triangle rectangle de côté a et d’hypoténuse h
   (en utilisant le théorème de Pythagore)
*)
let aire_triangle a h = failwith "Flemme de trouver l'équation déso"


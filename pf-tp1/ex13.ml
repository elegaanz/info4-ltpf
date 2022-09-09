open Ex3

let pi = 3.1415

let aire = function
  | Carre(a) -> a *. a
  | Rectangle(a, b) -> a *. b
  | Cercle(r) -> pi *. r *. r
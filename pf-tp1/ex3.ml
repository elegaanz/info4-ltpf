type semaine =
  | Lundi
  | Mardi
  | Mercredi
  | Jeudi
  | Vendredi
  | Samedi
  | Dimanche

type point2d = float * float

let origine_repere = (0., 0.)

type segment = point2d * point2d

(* les figures ne sont pas positionnées dans l'espace, on
   ne représente que leurs dimensions
*)
type figure =
  | Carre of float
  | Rectangle of float * float
  | Cercle of float


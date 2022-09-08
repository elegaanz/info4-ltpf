(* Un complexe est défini par deux flottants
   le premier -> partie réelle
   le deuxième -> partie imaginaire
*)
type complexe = float * float

let element_neutre = (0., 0.)

let add_complexe a b =
  let (re_a, im_a) = a in
  let (re_b, im_b) = b in
  (re_a +. re_b, im_a +. im_b)

(* version alternative en faisant la décompositon
   au moment où on défini les arguments *)

let add_complexe (re_a, im_a) (re_b, im_b) =
  (re_a +. re_b, im_a +. im_b)

let module_complexe (re, im) =
  sqrt (re *. re +. im *. im)

let oppose_complexe (re, im) =
  (-.re, -.im)

(* Version où le type complexe est un type somme avec un seul constructeur *)

type complexe = C of float * float

let element_neutre = C(0., 0.)

let add_complexe a b = match (a, b) with
  | C(re_a, im_a), C(re_b, im_b) -> (re_a +. re_b, im_a +. im_b)

let module_complexe (re, im) =
  sqrt (re *. re +. im *. im)

let oppose_complexe (re, im) =
  (-.re, -.im)
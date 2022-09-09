open Ex3

let (=~) = fun a b -> a -. b < epsilon_float
let (<&) = fun (a, b) (c, d) -> a < c && b < d 

let milieu_segment ((x1, y1), (x2, y2)) =
  (x1 +. ((x2 -. x1) /. 2.), y1 +. ((y2 -. y1) /. 2.))

let sur_segment ((x1, y1), (x2, y2)) (a, b) =
  let ratio (x1, y1) (x2, y2) = (y2 -. y1) /. (x2 -. x1) in
  let ratio1 = ratio (x1, y1) (a, b) in
  let ratio2 = ratio (x1, y1) (x2, y2) in

  ratio2 =~ ratio1 && (x1, y1) <& (a, b) && (a, b) <& (x2, y2)
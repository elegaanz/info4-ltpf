let assert_eq a b = assert (a = b)
let assert' exp = assert exp

let () =
  assert_eq 27. @@ Ex5.cube_flottant 3.;
  assert' @@ Ex5.est_positif 12;
  assert' @@ not @@ Ex5.est_positif (-12);
  assert' @@ Ex5.est_pair 10;
  assert' @@ not @@ Ex5.est_pair 7;
  assert_eq (-1) @@ Ex5.signe (-1351);
  assert_eq 0 @@ Ex5.signe 0;
  assert_eq 1 @@ Ex5.signe 5613;

  assert_eq 50 @@ Ex6.f1 5 2 5;
  assert_eq 15 @@ Ex6.f2 6 1 8;

  assert' @@ Ex7.est_triplet_pythagoricien 3 4 5;
  assert' @@ not @@ Ex7.est_triplet_pythagoricien 1 1 50;

  assert' @@ Ex7.meme_signe 10 165;
  assert' @@ Ex7.meme_signe (-58) (-631);
  assert' @@ not @@ Ex7.meme_signe 10 (-651);

  assert_eq 2 @@ Ex9.min2entiers 2 56;
  assert_eq 2 @@ Ex9.min2entiers 56 2;

  assert_eq 2 @@  Ex10.min3entiers 2 896 5;
  assert_eq 2 @@  Ex10.min3entiers 8645 2 653;
  assert_eq 2 @@  Ex10.min3entiers 855 51 2;

  let s = ((5., 5.), (15., 25.)) in
  assert_eq (10., 15.) @@ Ex11.milieu_segment s;
  assert' @@ Ex11.sur_segment s @@ Ex11.milieu_segment s
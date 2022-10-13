(** 2.1 FONCTIONS SIMPLES SUR UN ARBRE **)

type 'a abin = Node of 'a * 'a abin * 'a abin | Leaf

(* Arbre vide, parfois plus explicite que "leaf" *)
let abin_empty = Leaf

let test_abin = Node (0, Node (2, Leaf, Leaf), Node (10, Leaf, Node (-1, Leaf, Leaf)))

(* Hauteur et nombre de noeuds *)

let rec abin_node_count = function
  | Node (_, left, right) -> 1 + abin_node_count left + abin_node_count right
  | Leaf -> 0

let rec abin_height = function
  | Node (_, left, right) -> 1 + max (abin_height left) (abin_height right)
  | Leaf -> 0

let () =
  assert (abin_node_count test_abin = 4);
  assert (abin_height test_abin = 3)

(* Produit et somme *)

let rec abin_op op neutral = function
    | Node (label, left, right) -> op label (op (abin_op op neutral left) (abin_op op neutral right))
    | Leaf -> neutral

let abin_prod = abin_op ( * ) 1
let abin_sum = abin_op ( + ) 0

let () =
  assert (abin_prod Leaf = 1);
  assert (abin_sum Leaf = 0);
  assert (abin_prod (Node (3, Node (2, Leaf, Leaf), Leaf)) = 6);
  assert (abin_sum (Node (3, Node (2, Leaf, Leaf), Leaf)) = 5);
  assert (abin_prod test_abin = 0);
  assert (abin_sum test_abin = 11)

(* L'abre contient-il un élément ? *)

let rec abin_contains e = function
  | Node (label, left, right) -> (label = e) || (abin_contains e left) || (abin_contains e right)
  | Leaf -> false

let () =
  assert (not @@ abin_contains 13 test_abin);
  assert (abin_contains 10 test_abin);
  assert (abin_contains 0 test_abin)

(* Maximum (on pourrait faire les choses plus proprement en renvoyant un `int option`) *)

let abin_max = abin_op max Int.min_int

let () =
  assert (abin_max (Node (2, Leaf, Leaf)) = 2);
  assert (abin_max test_abin = 10)

(* Noeuds vraiment binaires *)

let rec abin_count_binary = function
  | Node (_, (Node _ as left), (Node _ as right)) -> 1 + abin_count_binary left + abin_count_binary right
  | _ -> 0

let () =
  assert (abin_count_binary test_abin = 1);
  assert (abin_count_binary Leaf = 0)

(** 2.2 ARBRES BINAIRES DE RECHERCHE **)

(* J'ai inventé ce terme pour désigner un noeud avec que des feuilles en enfant *)
let rameau label = Node (label, Leaf, Leaf)

let abr_test = Node (
  5,
  Node (3, rameau 2, Leaf),
  Node (8, Node (6, Leaf, rameau 7), rameau 9)
)

let rec abr_mem e = function
  | Leaf -> false
  | Node (label, left, right) -> (
    match Int.compare e label with
      | x when x < 0 -> abr_mem e left
      | x when x > 0 -> abr_mem e right
      | _ -> true
  )

let () =
  assert (abr_mem 2 abr_test);
  assert (abr_mem 5 abr_test);
  assert (abr_mem 6 abr_test);
  assert (not @@ abr_mem 1 abr_test);
  assert (not @@ abr_mem 42 Leaf)

let rec abr_insert e = function
  | Leaf -> rameau e
  | Node (label, left, right) -> (
    if e < label
    then Node (label, abr_insert e left, right)
    else Node (label, left, abr_insert e right)
  )

let () =
  assert (abr_mem 100 (abr_insert 100 abr_test));
  assert (abr_mem 0 (abr_insert 0 abr_test))

let abr_verif =
  let rec children_match pred = function
    | Leaf -> true
    | Node (label, left, right) -> pred label
      && children_match ((>) label) left
      && children_match ((<) label) right
  in children_match (fun _ -> true)

let _ = assert (abr_verif abr_test); assert (abr_verif Leaf)

(* Tri *)

let abr_insert_all base =
  List.fold_left (fun a b -> abr_insert b a) base

(* Transforme un arbre en une liste de ses éléments, par profondeur *)
let rec abin_collect_all = function
  | Leaf -> []
  | Node (label, left, right) -> List.concat [
    abin_collect_all left;
    [label];
    abin_collect_all right
  ]

let sort_via_abr list = list |> abr_insert_all abin_empty |> abin_collect_all

let () =
  assert (sort_via_abr [2; 0; 4; 3; 1] = [0; 1; 2; 3; 4]);
  assert (sort_via_abr [] = []);
  assert (sort_via_abr [-1; 1; 0] = [-1; 0; 1])

let rec abr_split e = function
  | Leaf -> failwith "split impossible: élément non trouvé"
  | Node (label, left, right) when e < label -> let (ll, lr) = abr_split e left in (
    ll, Node (label, lr, right)
  )
  | Node (label, left, right) when e > label -> let (rl, rr) = abr_split e right in (
    Node (label, left, rl), rr
  )
  | Node (label, left, right) -> (left, right)

let () =
  assert ((abr_split 5 abr_test) = (
    Node (3, rameau 2, Leaf),
    Node (8, Node (6, Leaf, rameau 7), rameau 9)
  ));
  assert ((abr_split 3 abr_test) = (
    rameau 2,
    Node (5, Leaf, Node (8, Node(6, Leaf, rameau 7), rameau 9))
  ))

let abr_merge first second = first |> abin_collect_all |> abr_insert_all second

let rec abr_suppr e abr =
  let (l, r) = abr_split e abr in
  abr_merge l r

let () =
  let assert_suppr n = assert (abr_test |> abr_suppr n |> abr_mem n |> not) in
  assert_suppr 2;
  assert_suppr 5

let rec abr_compare a b = match a with
| Leaf -> b = Leaf
| Node(x, g, d) ->
    try
      let (bg, bd) = abr_split x b in
      abr_compare g bg && abr_compare d bd
    with Failure _ -> false

let () =
    assert (abr_compare Leaf Leaf);
    assert (not @@ abr_compare Leaf (Node(0, Leaf, Leaf)));
    let a = (Node(
      4,
      Node(0, Leaf, Leaf),
      Node(7,
        Node(
          2,
          Leaf,
          Node(5, Leaf, Leaf)
        ),
        Leaf
      )
    )) in
    assert (abr_verif a);
    assert (abr_compare (abr_insert_all Leaf [2; 7]) (Node(
      2,
      Leaf,
      Node(7, Leaf, Leaf)
    )));
    assert (abr_compare (abr_insert_all Leaf [0; 5; 7; 4; 2]) a)
(**
Trois objectifs dans ce TD :
- Raisonner avec apply ... pour utiliser
  hypothèses implicatives et/ou quantifiées universellement)
- Utiliser des hypothèses de récurrence implicatives et/ou
   quantifiées universellement
- Utiliser une hypothèse indiquant une égalité entre deux 
  constructeurs différents, ce qui est "manifestement impossible"
*)

(* ---------------------------------------------------------------------- *)
(** * Partie 1 : raisonnement, tactiques refine et apply *)

(** ** Rappels sur les tactiques *)
(**

- intro x
  a le même effet que
  refine (fun x => _)

- apply f
  a le même effet que
  refine f, ou
  refine (f _), ou
  refine (f _ _), etc.
  suivant le type de [f].

  Exemple

  f : T1 -> T2
  ============
  T2

  On peut ici utiliser refine (f _), il restera à trouver
  quelque chose de type T1.

  - destruct E as [ (* Cons1 *) x | (* Cons2 *) | (* Cons3 *) y z]
    a le même effet que
    refine (match E with
             | Cons1 x => _
             | Cons2 => _
             | Cons3 y z => _
             end).

    où E a un type inductif de constructeurs Cons1 Cons2 et Cons3
    ayant respectivement 1, 0 et 2 arguments.
 *)

(** ** Rappels sur les structures en arbre *)
(**
    1) on a vu des structures de données destinées à la
    programmation (listes, etc.), et des structures de données
    destinées au raisonnement, appelées arbres de preuve,
    qui se manipulent de la même manière.

    2) On a vu également que les types de structures de données pour
    la programmation ont eux même un type, l'univers Set.
 *)

(** ** L'univers Prop *)
(**
    Les arbres de preuve sont dans un univers parallèle à Set,
    appelé Prop.

    Remarque : en particulier, les égalités sont dans Prop.
*)
Check (2 = 2).

Section sec_ABC.
  (** Considérons des propositions arbitraires A B C... *)
  Variable A B C : Prop.
  (** ... et des prédicats arbitraires P Q R sur nat *)
  Variable P Q R : nat -> Prop.

 (** Preuve à faire uniquement avec refine/intro/apply *)
 (** Faire également Show Proof après les invications de
     tactiques pour voir la preuve en train de se former. *)
  Lemma impl_trans:
    (A -> B) -> (B -> C) -> (A -> C).
  Proof.
    intro ab.
    intro bc.
    intro a.
    apply bc.
    apply ab.
    apply a.
  Qed.

  (** Preuve à faire uniquement avec intro/apply *)
  Lemma combi_S:   (A -> B -> C) -> (A -> B) -> A -> C.
  Proof.
    intro abc.
    intro ab.
    intro a.
    apply abc.
    apply a.
    apply ab.
    apply a.
  Qed.

  (* Preuve à faire uniquement avec intro/apply *)
  Lemma forall_impl_trans:
    (forall n:nat, P n -> Q n) ->
    (forall n:nat, Q n -> R n) ->
    (forall n:nat, P n -> R n).
  Proof.
    intro fa_pq.
    intro fa_qr.
    intro n.
    intro p.
    apply fa_qr.
    apply fa_pq.
    apply p.
  Qed.

End sec_ABC.


(* -------------------------------------------------------------------------- *)
(** * Partie 2 : équivalence entre égalité des entiers et eqnatb rendant true *)

(** eqnatb renvoie true ssi ses arguments représentent le même entier naturel *)

Fixpoint eqnatb n1 n2 :=
   match n1,n2 with
  | O,O => true
  | S n1', S n2' => eqnatb n1' n2'
  | _,_ => false
  end.

(** ** 2.1 Le sens le plus facile (récurrence simple) *)

Lemma eqnatb_eq_1 : forall n, eqnatb n n = true.
Proof.
  intros.
  induction n.
  - simpl. reflexivity.
  - cbn [eqnatb]. rewrite IHn. reflexivity.
Qed.

Lemma eqnatb_eq : forall n1 n2, n1 = n2 -> eqnatb n1 n2 = true.
Proof.
  intros. rewrite H. rewrite eqnatb_eq_1. reflexivity.
Qed.

(* Facultatif : preuve directe sans utiliser eqnatb_eq_1 *)
Lemma eqnatb_eq_direct : forall n1 n2, n1 = n2 -> eqnatb n1 n2 = true.
Proof.
  intros.
  rewrite H.
  clear H.
  induction n2.
  - cbn [eqnatb]. reflexivity.
  - cbn [eqnatb]. rewrite IHn2. reflexivity.
Qed.
(** ** 2.2 Le sens le plus difficile (récurrence quantifiée + preuve par cas) *)

(*** 2.2.1 Lemme préparatoire *)

(** Nouvelle tactique :
la tactique [change _expr_] permet de remplacer la conclusion
par une autre propriété convertible c'est-à-dire equivalente par calcul.

exemple:

===========
2 + 1 = 6
-> change (3 = 3 * 2)

Cela permet de "décalculer" un résultat de fonction.
 *)

Lemma absurd: 5 = 4 -> 15 = 12.
Proof.
  intros e.
  change (5 * 3 = 4 * 3).
  rewrite e. cbn ["*" "+"]. reflexivity.
Qed.


Definition f b (n1 : nat) n2 := match b with
| true => n1
| false => n2
end.

(* facultatif *)
Lemma true_false_eg : true = false -> forall n1 n2 : nat, n1 = n2.
Proof.
  intro etf. intros n1 n2.
  pose (f (b : bool) := if b then n1 else n2).
  change (f true = f false).
  rewrite etf.
  reflexivity.
  (** Definir une fonction f tq [f true = n1] et [f false = n2]   *)
  (* À compléter *)
  (* pose (f (b:bool) := ... *)
Qed.

(*** 2.2.2 Réciproque de 2.1 *)
(** Dans l'exercice ci-dessous, il faut bien identifier la propriété de n1
    sur laquelle porte la récurrence *)
Lemma eq_eqnatb : forall n1 n2, eqnatb n1 n2 = true -> n1 = n2.
Proof.
  intros n1.
  induction n1.
  - intro n2. destruct n2.
    + reflexivity.
    + cbn [eqnatb]. intros. symmetry in H. apply true_false_eg. exact H.
  - intro n2. induction n2.
    + cbn [eqnatb]. intros. symmetry in H. apply true_false_eg. exact H.
    + intros. cbn [eqnatb] in H. apply IHn1 in H. apply f_equal. exact H.
Qed.

(** ** 2.3 Équivalence, tactique split *)

(** On peut utiliser split pour prouver une équivalence
    par décomposition en deux implications *)

Lemma eq_iff_eqnatb : forall n1 n2, eqnatb n1 n2 = true <-> n1 = n2.
Proof.
  split.
  apply eq_eqnatb.
  apply eqnatb_eq.
Qed.


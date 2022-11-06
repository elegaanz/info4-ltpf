(* Pour les tests *)

let list_of_string s =
  let n = String.length s in
  let rec boucle i = if i = n then [] else s.[i] :: boucle (i + 1) in
  boucle 0

(* Le type des aspirateurs (fonctions qui aspirent le préfixe d'une liste) *)
type analist = char list -> char list

(* Le type des aspirateurs qui, en plus, rendent un résultat *)
type 'r ranalist = char list -> 'r * char list

(* Idem en plus général (utile lorsqu'on enchaîne analyse lexicale puis syntaxique). *)
type ('token, 'res) ranalist_gen = 'token list -> 'res * 'token list

exception Echec

(* Aspirateur d'un caractère précis en début de liste *)
let terminal c l = match l with x :: l when x = c -> l | _ -> raise Echec

let terminal_cond (p : char -> bool) : analist =
 fun l -> match l with x :: l when p x -> l | _ -> raise Echec

(* Le même avec résultat *)
let terminal_res (f : 'term -> 'res option) : 'res ranalist =
 fun l ->
  match l with
  | x :: l -> ( match f x with Some y -> (y, l) | None -> raise Echec)
  | _ -> raise Echec

(* Non-terminal vide *)
let epsilon : analist = fun l -> l
let epsilon_res (res : 'r) : 'r ranalist = fun l -> (res, l)

(* Avant de définir le type coup on définit le type joueur *)
type joueur = Blanc | Noir
type coup = joueur * int * int
type partie = int * int * coup list

type token =
  | ParenG (* parenthèse gauche *)
  | ParenD (* parenthèse droite *)
  | BlancT
    (* On doit les appeler BlancT / NoirT pour éviter les conflits avec les constructeurs du type joueur (T pour token) *)
  | NoirT
  | Int of int
(* On ne va pas faire un constructeur par entier possible vu qu'il y en a UNE INFINITÉ et que ça fait beaucoup, donc on fait un constructeur avec un `int` en paramètre *)

let reco_pareng chaine =
  match chaine with '(' :: suite -> (ParenG, suite) | _ -> raise Echec

let reco_parend chaine =
  match chaine with ')' :: suite -> (ParenD, suite) | _ -> raise Echec

let reco_noir chaine =
  match chaine with
  | 'n' :: 'o' :: 'i' :: 'r' :: suite -> (NoirT, suite)
  | _ -> raise Echec

let reco_blanc chaine =
  match chaine with
  | 'b' :: 'l' :: 'a' :: 'n' :: 'c' :: suite -> (BlancT, suite)
  | _ -> raise Echec

(* On considère qu'on a un analyseur lexical pour les entiers
     parse_int : char list -> int * char list
   grâce à la question 6.1
*)
let parse_int = function
  | [] -> raise Echec
  | '0' :: xs -> (0, xs)
  | '1' :: xs -> (1, xs)
  | '2' :: xs -> (2, xs)
  | '3' :: xs -> (3, xs)
  | '4' :: xs -> (4, xs)
  | '5' :: xs -> (5, xs)
  | '6' :: xs -> (6, xs)
  | '7' :: xs -> (7, xs)
  | '8' :: xs -> (8, xs)
  | _ :: xs -> (9, xs)

let reco_int chaine =
  let i, reste = parse_int chaine in
  Int i

let token_here chaine =
  try
    reco_pareng chaine
  with Echec -> try
    reco_parend chaine
  with Echec -> try
    reco_blanc chaine
  with Echec -> try
    reco_noir chaine
  with Echec ->
    let i, reste = parse_int chaine in
    (Int i, reste)

let rec aspi_espace chaine =
  match chaine with ' ' :: suite -> aspi_espace suite | _ -> chaine

let next_token chaine = token_here (aspi_espace chaine)

let rec tokens_connect6 chaine =
  if chaine = [] then []
  else
    let token, reste = next_token chaine in
    token :: tokens_connect6 reste

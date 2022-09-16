type listeint = Nil | Cons of int * listeint

let rec len = function
| Nil -> 0
| Cons(_, xs) -> 1 + len xs

let rec len = function
| [] -> 0
| x :: xs -> 1 + len xs

type 'a liste = Nil | Cons of 'a * 'a liste

let rec len = function
| Nil -> 0
| Cons(_, xs) -> 1 + len xs
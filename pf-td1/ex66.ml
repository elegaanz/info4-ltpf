type abin = F | N of abin * int * abin

let rec nb_noeuds = function
| F -> 0
| N(g, x, d) -> 1 + (nb_noeuds g) + (nb_noeuds d)

let rec hauteur = function
| F -> 0
| N(g, x, d) -> 1 + max (hauteur g) (hauteur d)

let rec fold f x = function
| F -> x
| N(g, e, d) -> f (fold f x g) e (fold f x d)

let bifold f = fold (fun a x -> f (f a x)) 

let prod = bifold ( * ) 1
let sum = bifold ( + ) 0

let contains e = fold (fun a b c -> a || b = e || c) false

let max_abin = fold (fun a b c -> )
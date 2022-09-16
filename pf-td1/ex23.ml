let rec appartient e = function
| [] -> false
| x :: xs -> x = e || appartient e xs
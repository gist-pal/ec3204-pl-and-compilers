type exp =
  | Int of int
  | Minus of exp * exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Div of exp * exp

let rec eval : exp -> int
= fun exp ->
  match exp with
  | Int n -> n
  | Minus (e1,e2) -> eval e1 - eval e2
  | Plus (e1,e2) -> eval e1 + eval e2
  | Mult (e1,e2) -> eval e1 * eval e2
  | Div (e1,e2) ->
    try eval e1 / eval e2
    with Division_by_zero -> failwith "eval e2 is zero"

let exp1 = Int 3
let exp2 = Plus (Int 3, Plus (Int 5, Mult (Int 2, Int 3)))
let exp3 = Div (Plus (Int 1, Int 4), Minus (Int 3, Mult (Int 1, Int 3)))

let _ = print_endline (string_of_int (eval exp1))
let _ = print_endline (string_of_int (eval exp2))
let _ = print_endline (string_of_int (eval exp3))

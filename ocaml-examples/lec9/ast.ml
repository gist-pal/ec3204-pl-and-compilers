type expr =
  | Num of int
  | Add of expr * expr
  | Mul of expr * expr
  | Minus of expr * expr
  | Div of expr * expr

let rec eval : expr -> int
= fun e ->
  match e with
  | Num n -> n
  | Add (e1, e2) -> (eval e1) + (eval e2)
  | Mul (e1, e2) -> (eval e1) * (eval e2)
  | Minus (e1, e2) -> (eval e1) - (eval e2)
  | Div (e1, e2) ->
    if eval e2 == 0 then raise (Failure "division by zero")
    else (eval e1) / (eval e2)

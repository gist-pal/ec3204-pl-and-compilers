open Vocab

type pgm = fid * param list * cmd
and fid = string
and param = typ * var
and var = string

and typ =
  | T_Int
  | T_Array of int

and lv =
  | Var of var
  | Arr of var * aexp

and aexp =
  | Int of int
  | Len of var
  | Lv of lv
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | Mul of aexp * aexp

and bexp =
  | True
  | False
  | Eq of aexp * aexp
  | Neq of aexp * aexp
  | Leq of aexp * aexp
  | Lt of aexp * aexp
  | Geq of aexp * aexp
  | Gt of aexp * aexp
  | Not of bexp
  | Or of bexp * bexp
  | And of bexp * bexp

and cmd =
  | Decl of typ * var
  | Assign of lv * aexp
  | Skip
  | Seq of cmd * cmd
  | If of bexp * cmd * cmd
  | While of bexp * cmd
  | Assert of bexp

let rec to_string_typ : typ -> string
= fun t ->
  match t with
  | T_Int -> "int"
  | T_Array n -> "int" ^ "[" ^ string_of_int n ^ "]"

let rec to_string_cmd ?(indent="") : cmd -> string
= fun cmd ->
  match cmd with
  | Decl (t,x) -> indent ^ to_string_typ t ^ " " ^ x ^ ";"
  | Assign (lv,a) -> indent ^ to_string_lv lv ^ " = " ^ to_string_aexp a ^ ";"
  | Skip -> indent ^ "skip;"
  | Assert b -> indent ^ "assert" ^ "(" ^ to_string_bexp b ^ ")" ^ ";"
  | Seq (c1,c2) -> to_string_cmd ~indent c1 ^ "\n" ^ to_string_cmd ~indent c2
  | If (b,c1,c2) ->
    indent ^
    "if" ^ "(" ^ to_string_bexp b ^ ")" ^ "{" ^ "\n" ^
    to_string_cmd ~indent:("  " ^ indent) c1 ^ "\n" ^
    indent ^ "}" ^ "\n" ^
    indent ^
    "else" ^ "{" ^ "\n" ^
    to_string_cmd ~indent:("  " ^ indent) c2 ^ "\n" ^
    indent ^ "}"
  | While (b,c) ->
    indent ^ "while" ^ " " ^ "(" ^ to_string_bexp b ^ ")" ^ " " ^ "{\n" ^
    to_string_cmd ~indent:("  " ^ indent) c ^ "\n" ^
    indent ^ "}"

and to_string_aexp : aexp -> string
= fun ae ->
  match ae with
  | Int n -> string_of_int n
  | Len x -> "len(" ^ x ^ ")"
  | Lv lv -> to_string_lv lv
  | Plus (a1,a2) -> to_string_aexp a1 ^ " + " ^ to_string_aexp a2
  | Minus (a1,a2) -> to_string_aexp a1 ^ " - " ^ to_string_aexp a2
  | Mul (a1,a2) -> to_string_aexp a1 ^ " * " ^ to_string_aexp a2

and to_string_bexp : bexp -> string
= fun be ->
  match be with
  | True -> "true"
  | False -> "false"
  | Eq (a1,a2) ->  to_string_aexp a1 ^ " == " ^ to_string_aexp a2
  | Neq (a1,a2) -> to_string_aexp a1 ^ " != " ^ to_string_aexp a2
  | Leq (a1,a2) -> to_string_aexp a1 ^ " <= " ^ to_string_aexp a2
  | Lt (a1,a2) ->  to_string_aexp a1 ^ " < "  ^ to_string_aexp a2
  | Geq (a1,a2) -> to_string_aexp a1 ^ " >= " ^ to_string_aexp a2
  | Gt (a1,a2) ->  to_string_aexp a1 ^ " > "  ^ to_string_aexp a2
  | Not b -> "!" ^ "(" ^ to_string_bexp b ^ ")"
  | Or (b1,b2) -> "(" ^ to_string_bexp b1 ^ " || " ^ to_string_bexp b2 ^ ")"
  | And (b1,b2) -> "(" ^ to_string_bexp b1 ^ " && " ^ to_string_bexp b2 ^ ")"

and to_string_lv : lv -> string
= fun lv ->
  match lv with
  | Var x -> x
  | Arr (x,a) -> x ^ "[" ^ to_string_aexp a ^ "]"

let to_string_param (typ,var) = to_string_typ typ ^ " " ^ var

let to_string_params params =
  string_of_list ~first:"(" ~sep:", " ~last:")" to_string_param params

let to_string_pgm (fname,params,cmd) =
  fname ^ " " ^ to_string_params params ^ " " ^ "{\n" ^
    to_string_cmd ~indent:"  " cmd ^ "\n" ^
  "}\n"

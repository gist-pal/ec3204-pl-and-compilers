(****************************)
(*** abstract syntax of S ***)
(****************************)

type program = block
and block = decls * stmts
and decls = decl list
and decl  = typ * id 
and typ   = TINT | TARR of int
and stmts = stmt list 
and id    = string
and stmt  = ASSIGN of lv * exp
          | IF of exp * stmt * stmt 
          | WHILE of exp * stmt
          | DOWHILE of stmt * exp
          | READ of id
          | PRINT of exp 
          | BLOCK of block
and lv   = ID of id | ARR of id * exp
and exp  =  ICONST of int (* integer constant *)
          | LV of lv
          | ADD of exp * exp
          | SUB of exp * exp
          | MUL of exp * exp
          | DIV of exp * exp
          | UMINUS of exp
          | NOT of exp
          | LT of exp * exp 
          | LE of exp * exp 
          | GT of exp * exp 
          | GE of exp * exp 
          | EQ of exp * exp 
          | AND of exp * exp
          | OR  of exp * exp

(*************************************)
(*        interpreter for S          *)
(*************************************)

type loc = VAR of string | ADDR of base * offset
and base = int
and offset = int
type value = INT of int | ARRAY of base * size
and size = int

let str_of_loc l = 
  match l with
  | VAR x -> x
  | ADDR (x,n) -> "(l"^(string_of_int x)^","^(string_of_int n)^")"

let new_loc = ref 1
module Memory = struct
  type t = (loc, value) BatMap.t
  let empty = BatMap.empty
  let bind l v m = BatMap.add l v m
  let lookup l m = try BatMap.find l m 
                   with _ -> raise (Failure ("Memory error: " ^ (str_of_loc l)))
  let alloc x size m = 
    if size <= 0 then raise (Failure "alloc with non-positive size")
    else begin
    new_loc := !new_loc + 1;
    let rec helper offset m = 
      if offset = size then m
      else helper (offset+1) (bind (ADDR (!new_loc,offset)) (INT 0) m) in
      bind (VAR x) (ARRAY (!new_loc, size)) (helper 0 m)
    end
end

type mem = Memory.t

let list_fold f l a = List.fold_left (fun a e -> f e a) a l

let rec run_block :block -> mem -> mem
= fun (decls,stmts) m ->
  let m' = run_decls decls m in
  let m'' = run_stmts stmts m' in
    m''

and run_decls : decls -> mem -> mem
= fun decls m -> list_fold run_decl decls m

and run_decl : decl -> mem -> mem
= fun (typ,x) m -> 
  match typ with
  | TINT -> Memory.bind (VAR x) (INT 0) m
  | TARR n -> Memory.alloc x n m

and run_stmts : stmts -> mem -> mem
= fun stmts m -> list_fold run_stmt stmts m

and run_stmt : stmt -> mem -> mem
= fun stmt m ->
  match stmt with
  | ASSIGN (lv, e) -> Memory.bind (eval_lv lv m) (eval e m) m
  | IF (e,stmt1,stmt2) ->
    (match eval e m with
    | INT 0 -> run_stmt stmt2 m
    | INT _ -> run_stmt stmt1 m
    | _ -> failwith "Error: IF")
  | WHILE (e,stmt) ->
    (match eval e m with
     | INT 0 -> m
     | INT _ ->
       let m1 = run_stmt stmt m in
       run_stmt (WHILE (e,stmt)) m1
     | _ -> failwith "Error : while")
  | DOWHILE (stmt,e) ->
    let m1 = run_stmt stmt m in
    (match eval e m1 with
     | INT 0 -> m1
     | INT _ -> run_stmt (DOWHILE (stmt,e)) m1
     | _ -> failwith "Error: dowhile")
  | READ x -> Memory.bind (VAR x) (INT (read_int ())) m
  | PRINT e -> 
    (match eval e m with
     | INT n -> print_endline (string_of_int n); m
     | _ -> failwith "print: not an integer")
  | BLOCK b -> run_block b m

and eval_int : exp -> mem -> int 
= fun e m ->
  match eval e m with
  | INT n -> n
  | _ -> failwith "Expression must evaluate to integer"

and eval : exp -> mem -> value
= fun e m ->
  match e with
  | ICONST n -> INT n
  | LV lv -> Memory.lookup (eval_lv lv m) m
  | ADD (e1,e2) -> INT ((eval_int e1 m) + (eval_int e2 m))
  | SUB (e1,e2) -> INT ((eval_int e1 m) - (eval_int e2 m))
  | MUL (e1,e2) -> INT ((eval_int e1 m) * (eval_int e2 m))
  | DIV (e1,e2) -> INT ((eval_int e1 m) / (eval_int e2 m))
  | UMINUS e -> INT (-(eval_int e m))
  | NOT e -> 
    (match eval_int e m with
    | 0 -> INT 1
    | _ -> INT 0)
  | LT (e1,e2) -> if eval_int e1 m <  eval_int e2 m then INT 1 else INT 0
  | LE (e1,e2) -> if eval_int e1 m <= eval_int e2 m then INT 1 else INT 0
  | GT (e1,e2) -> if eval_int e1 m >  eval_int e2 m then INT 1 else INT 0
  | GE (e1,e2) -> if eval_int e1 m >= eval_int e2 m then INT 1 else INT 0
  | EQ (e1,e2) -> if eval_int e1 m =  eval_int e2 m then INT 1 else INT 0
  | AND (e1,e2) -> 
    (match eval_int e1 m, eval_int e2 m with
    |0,_ 
    |_,0 -> INT 0
    |_,_ -> INT 1)
  | OR (e1,e2) ->
    (match eval_int e1 m, eval_int e2 m with
    |0,0 -> INT 0
    |_,_ -> INT 1)

and eval_lv : lv -> mem -> loc
= fun lv m ->
  match lv with
  | ID x -> VAR x
  | ARR (x,e) -> 
    (match Memory.lookup (VAR x) m with
    | ARRAY (base,size) -> 
      (match eval e m with
       | INT idx -> 
         if idx < 0 || idx >= size then raise (Failure ("Array out of bounds: offset: " ^
         string_of_int idx ^ " size: " ^ string_of_int size)) 
         else ADDR (base, idx)
       | _ -> raise (Failure ("index must be an integer")))
    | _ -> raise (Failure (x ^ " must be an array")))


let execute : program -> unit
= fun pgm -> ignore (run_block pgm Memory.empty)

(*************************************)
(* pretty printer for the S langauge *)
(*************************************)

let p x = print_string (x)

let rec p_indent n = if n = 0 then () else (p " "; p_indent (n-1))

let rec p_typ t =
  match t with
  | TINT  -> p "int"
  | TARR (n) -> p "int"; p"[";print_int n; p"]"

and p_lv lv = 
  match lv with
  | ID x -> p x
  | ARR (x, e) -> p x; p "["; p_exp e; p "]"

and p_exp e = 
  begin
  match e with
  | ADD (e1,e2) -> p_exp e1; p "+"; p_exp e2
  | SUB (e1,e2) -> p_exp e1; p "-"; p_exp e2
  | MUL (e1,e2) -> p_exp e1; p "*"; p_exp e2
  | DIV (e1,e2) -> p_exp e1; p "/"; p_exp e2
  | UMINUS e -> p "-"; p_exp e
  | LV lv -> p_lv lv;
  | ICONST i -> print_int i
  | LT (e1,e2) -> p_exp e1; p "<"; p_exp e2
  | LE (e1,e2) -> p_exp e1; p "<="; p_exp e2
  | GT (e1,e2) -> p_exp e1; p ">"; p_exp e2
  | GE (e1,e2) -> p_exp e1; p ">="; p_exp e2
  | EQ (e1,e2) -> p_exp e1; p "=="; p_exp e2
  | NOT e -> p "!"; p_exp e
  | AND (e1,e2) -> p_exp e1; p"&&"; p_exp e2
  | OR (e1,e2) -> p_exp e1; p"||"; p_exp e2
  end
  
and p_decl : int -> decl -> unit
= fun indent (typ,var) -> 
  p_indent indent;
  p_typ typ; p " "; p var; p ";"

and p_stmt : int -> stmt -> unit
= fun indent stmt  -> 
  p_indent indent;
  begin
  match stmt with
  | ASSIGN (lv, exp) -> p_lv lv; p " = "; p_exp exp; p ";"
  | IF (bexp,stmt1,stmt2) -> p "if "; p_exp bexp; p ""; p_stmt indent stmt1; p "else"; p_stmt indent stmt2
  | WHILE (b, s) -> p "while "; p_exp b; p ""; p_stmt indent s
  | DOWHILE (s,b) -> p "do"; p_stmt (indent+1) s; p_indent indent; p "while"; p_exp b; p";"
  | PRINT e -> p "print "; p_exp e; p ""; p";"
  | READ x -> p "read "; p x; p ""; p";"
  | BLOCK b -> p_block indent b
  end;
  
and p_decls : int -> decls -> unit
= fun indent decls -> List.iter (fun decl -> p_decl indent decl; p "\n") decls

and p_stmts : int -> stmts -> unit
= fun indent stmts -> List.iter (fun stmt -> p_stmt indent stmt; p "\n") stmts

and p_block : int -> block -> unit
= fun indent (decls,stmts) ->
  p_indent indent; p "{\n";
  p_decls (indent + 1) decls;
  p_stmts (indent + 1) stmts;
  p_indent indent; p "}\n" 

let pp : program -> unit
= fun b -> p_block 0 b

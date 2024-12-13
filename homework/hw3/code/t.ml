(***********************************)
(* abstract syntax definition of T *)
(***********************************)

type program = linstr list
and linstr = label * instr (* labeled instruction *)
and instr = 
  | SKIP
  | ALLOC of var * int  (* x = alloc(n) *)
  | ASSIGNV of var * bop * var * var (* x = y bop z *)
  | ASSIGNC of var * bop * var * int (* x = y bop n *)
  | ASSIGNU of var * uop * var  (* x = uop y *)
  | COPY of var * var           (* x = y *)
  | COPYC of var * int          (* x = n *)
  | UJUMP of label              (* goto L *)
  | CJUMP of var * label        (* if x goto L *)
  | CJUMPF of var * label       (* ifFalse x goto L *)
  | LOAD of var * arr           (* x = a[i] *)
  | STORE of arr * var          (* a[i] = x *)
  | READ of var                 (* read x *)
  | WRITE of var                (* write x *)
  | HALT
and var = string
and label = int
and arr = var * var
and bop = ADD | SUB | MUL | DIV | LT | LE | GT | GE | EQ | AND | OR
and uop = MINUS | NOT

let dummy_label = 0

(*************************************)
(*        Interpreter for T          *)
(*************************************)
type loc = VAR of var | ADDR of base * offset
and value = INT of int | ARRAY of base * size
and offset = int
and size = int
and base = int

let string_of_value v = 
  match v with
  | INT n -> string_of_int n
  | ARRAY (b,s) -> "array of size " ^ string_of_int s

let rec range b e = if b = e then [b] else b::(range (b+1) e)

let new_loc = ref 1
module Memory = struct
  type t = (loc, value) BatMap.t
  let empty = BatMap.empty 
  let alloc : t -> int -> t * (base * size)
  = fun mem n -> 
    new_loc := !new_loc + 1; 
    (List.fold_left (fun m offset ->
        BatMap.add (ADDR (!new_loc, offset)) (INT 0) m
     ) mem (range 0 (n-1)), (!new_loc,n))
  let bind = BatMap.add
  let lookup a m = try BatMap.find a m with _ -> raise (Failure "Memory error")
end

type state = pc * Memory.t
and pc = int
and l2pc = (label,pc) BatMap.t
and global = program * l2pc

let eval_binary : value -> bop -> value -> value
= fun v1 op v2 ->
  match v1,op,v2 with
  | INT n1, ADD, INT n2 -> INT (n1+n2)
  | INT n1, SUB, INT n2 -> INT (n1-n2)
  | INT n1, MUL, INT n2 -> INT (n1*n2)
  | INT n1, DIV, INT n2 -> INT (n1/n2)
  | INT n1, LT, INT n2 -> if n1 < n2 then INT 1 else INT 0
  | INT n1, LE, INT n2 -> if n1 <= n2 then INT 1 else INT 0
  | INT n1, GT, INT n2 -> if n1 > n2 then INT 1 else INT 0
  | INT n1, GE, INT n2 -> if n1 >= n2 then INT 1 else INT 0
  | INT n1, EQ, INT n2 -> if n1 = n2 then INT 1 else INT 0
  | INT n1, AND, INT n2 -> if n1 != 0 && n2 != 0 then INT 1 else INT 0
  | INT n1, OR, INT n2 -> if n1 != 0 || n2 != 0 then INT 1 else INT 0
  | _ -> raise (Failure "T.eval_binary : invalid operands")

let eval_unary : uop -> value -> value
= fun op v ->
  match op, v with
  | MINUS, INT n -> INT (-n)
  | NOT, INT n -> if n = 0 then INT 1 else INT 0
  | _ -> raise (Failure "T.eval_unary: invalid operands")

let get_array_location : arr -> Memory.t -> loc
= fun (a,x) mem ->
  match Memory.lookup (VAR a) mem with
  | ARRAY (base, size) -> 
    begin
      match Memory.lookup (VAR x) mem with
      | INT offset -> 
        if offset < 0 || offset >= size then raise (Failure "T: invalid array index") 
        else ADDR (base, offset)
      | _ -> raise (Failure "T: invalid array index")
    end
  | _ -> raise (Failure "T: invalid array")

let get_instr : program -> pc -> instr
= fun pgm pc -> 
  let (_,instr) = List.nth pgm pc in
    instr

let get_next_mem : instr -> Memory.t -> Memory.t
= fun instr mem -> 
  match instr with
  | ALLOC (x,n) -> 
    let mem, (b,s) = Memory.alloc mem n in
      Memory.bind (VAR x) (ARRAY (b,s)) mem
  | ASSIGNV (x,o,y,z) ->
    let v_y = Memory.lookup (VAR y) mem in
    let v_z = Memory.lookup (VAR z) mem in
    let v_x = eval_binary v_y o v_z in
      Memory.bind (VAR x) v_x mem
  | ASSIGNC (x,o,y,n) ->
    let v_y = Memory.lookup (VAR y) mem in
    let v_x = eval_binary v_y o (INT n) in
      Memory.bind (VAR x) v_x mem
  | ASSIGNU (x,o,y) ->
    let v_y = Memory.lookup (VAR y) mem in
    let v_x = eval_unary o v_y in
      Memory.bind (VAR x) v_x mem
  | COPY (x,y) -> Memory.bind (VAR x) (Memory.lookup (VAR y) mem) mem
  | COPYC (x,c) -> Memory.bind (VAR x) (INT c) mem
  | LOAD (x,arr) ->
    let arrloc = get_array_location arr mem in
    let v_x = Memory.lookup arrloc mem in
      Memory.bind (VAR x) v_x mem
  | STORE (arr,x) ->
    let v_x = Memory.lookup (VAR x) mem in
    let arrloc = get_array_location arr mem in
      Memory.bind arrloc v_x mem
  | READ x ->
    let v = read_int () in
      Memory.bind (VAR x) (INT v) mem
  | WRITE x ->
    let v_x = Memory.lookup (VAR x) mem in
    (match v_x with
    | INT n -> print_endline (string_of_int n)
    | _ -> raise (Failure "WRITE: not an integer")
    );
    mem
  | _ -> mem

let get_pc_of_label : global -> label -> pc
= fun (_,l2pc) l -> try BatMap.find l l2pc with _ -> raise (Failure ("T: Label not found :"^ string_of_int l))
  
let get_next_pc  : global -> instr -> state -> pc
= fun global instr (pc,mem) -> 
  match instr with
  | UJUMP l -> get_pc_of_label global l 
  | CJUMP (x,l) -> 
    let v = Memory.lookup (VAR x) mem in 
      begin
      match v with
      | INT n -> if n != 0 then get_pc_of_label global l else pc+1
      | _ -> raise (Failure "T: invalid operands for CJUMP") 
      end
  | CJUMPF (x,l) -> 
    let v = Memory.lookup (VAR x) mem in 
      begin
      match v with
      | INT n -> if n = 0 then get_pc_of_label global l else pc+1
      | _ -> raise (Failure "T: invalid operands for CJUMP") 
      end
  | _ -> pc+1
 
let run : global -> state -> state
= fun (pgm,l2pc) (pc,mem) -> 
  let instr = get_instr pgm pc in
  let mem' = get_next_mem instr mem in
  let pc'  = get_next_pc (pgm,l2pc) instr (pc,mem) in
    (pc',mem')

let rec loop : global -> state -> int -> unit
= fun (pgm,l2pc) (pc,mem) k -> 
  if get_instr pgm pc = HALT then print_endline ("#instructions executed : " ^ string_of_int k)
  else loop (pgm,l2pc) (run (pgm,l2pc) (pc,mem)) (k+1)

let get_label2pc : program -> l2pc 
= fun pgm -> 
  let _,map =
    List.fold_left (fun (pc,map) (l,_) ->
      if l = dummy_label then (pc+1,map) else (pc+1,BatMap.add l pc map)
    ) (0,BatMap.empty) pgm in
  map

let execute : program -> unit
= fun pgm -> 
  let l2pc = get_label2pc pgm in
    loop (pgm,l2pc) (0,Memory.empty) 0

(*************************************)
(* pretty printer for the T language *)
(*************************************)
let pp : program -> unit
= fun pgm ->
  let ps = print_string in
  let pn = print_endline in
  let s_bop o = match o with ADD -> "+" | SUB -> "-" | MUL -> "*" | DIV -> "/"
  | LT -> "<" | LE -> "<=" | GT -> ">" | GE -> ">=" | EQ -> "==" | AND -> "&&" |
  OR -> "||" in
  let s_uop o = match o with MINUS -> "-" | NOT -> "!" in
  let s_arr (x,y) = x ^ "[" ^ y ^ "]" in
  List.iter (fun (label, instr) ->
    ps (string_of_int label ^ " : ");
    match instr with
    | HALT -> pn "HALT"
    | SKIP -> pn "SKIP"
    | ALLOC (x,n) -> pn (x ^ " = alloc (" ^ string_of_int n ^ ")")
    | ASSIGNV (x,o,y,z) -> pn (x ^ " = " ^ y ^ " " ^ s_bop o ^ " " ^ z)
    | ASSIGNC (x,o,y,n) -> pn (x ^ " = " ^ y ^ " " ^ s_bop o ^ " " ^ string_of_int n)
    | ASSIGNU (x,o,y) -> pn (x ^ " = " ^ s_uop o ^ y)
    | COPY (x,y) -> pn (x ^ " = " ^ y)
    | COPYC (x,n) -> pn (x ^ " = " ^ string_of_int n)
    | UJUMP label -> pn ("goto " ^ string_of_int label)
    | CJUMP (x,l) -> pn ("if " ^ x ^ " goto " ^ string_of_int l)
    | CJUMPF (x,l) -> pn ("iffalse " ^ x ^ " goto " ^ string_of_int l)
    | LOAD (x,a) -> pn (x ^ " = " ^ s_arr a)
    | STORE (a,x) -> pn (s_arr a ^  " = " ^ x)
    | READ x -> pn ("read " ^ x)
    | WRITE x -> pn ("write " ^ x)
  ) pgm

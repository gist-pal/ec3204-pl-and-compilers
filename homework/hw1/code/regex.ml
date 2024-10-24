type t =
  | Empty
  | Epsilon
  | Symbol of symbol
  | SymbolSet of symbol * symbol (* regex shorthand. SymbolSet (c1,c2) = [c1-c2] *)
  | OR of t * t
  | CONCAT of t * t
  | STAR of t
  | POS of t

and symbol = char

let string_of_symbol = Char.escaped

(* enumerate symbols within a range *)
(* e.g., range 'a' 'd' = ['a';'b';'c';'d'] *)
let range : symbol -> symbol -> symbol list
= fun a b ->
  List.init (Char.code b - Char.code a + 1) (fun i -> Char.chr (Char.code a + i))

(* a set of input symbols: a-z, A-Z, 0-9, _ *)
let alphabet : symbol list =
  range 'a' 'z' @ range 'A' 'Z' @ range '0' '9' @ ['_']

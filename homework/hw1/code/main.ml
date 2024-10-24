open Regex
open Hw

let testcases : (Regex.t * symbol list) list =
  [
    (Empty, []);
    (Epsilon, []);
    (Symbol 'a', ['a']);
    (Symbol 'b', ['b']);
    (OR (Symbol 'a', Symbol 'b'), ['b']);
    (CONCAT (STAR (Symbol 'a'), Symbol 'b'), ['b']);
    (CONCAT (STAR (Symbol 'a'), Symbol 'b'), ['a';'b']);
    (CONCAT (STAR (Symbol 'a'), Symbol 'b'), ['a';'a';'b']);
    (CONCAT (STAR (Symbol 'a'), Symbol 'b'), ['a';'b';'b']);
    (CONCAT (STAR (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['b']);
    (CONCAT (STAR (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['a';'a';'b']);
    (CONCAT (STAR (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['a';'a';'a';'b']);
    (CONCAT (STAR (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['a';'a';'a';'a';'b']);
    (CONCAT (POS (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['b']);
    (CONCAT (POS (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['a';'a';'b']);
    (CONCAT (POS (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['a';'a';'a';'b']);
    (CONCAT (POS (CONCAT (Symbol 'a', Symbol 'a')), Symbol 'b'), ['a';'a';'a';'a';'b']);
    (CONCAT (POS (OR (SymbolSet ('a','z'), SymbolSet ('A', 'Z'))), STAR (Symbol '_')), ['g';'i';'s';'t']);
    (CONCAT (POS (OR (SymbolSet ('a','z'), SymbolSet ('A', 'Z'))), STAR (Symbol '_')), ['G';'i';'S';'t']);
    (CONCAT (POS (OR (SymbolSet ('a','z'), SymbolSet ('A', 'Z'))), STAR (Symbol '_')), ['G';'i';'_';'s';'t']);
  ]

let match_regex : Regex.t -> symbol list -> bool * bool
= fun regex input ->
  let nfa = Hw.regex2nfa regex in
  let dfa = Hw.nfa2dfa nfa in
  (Hw.run_nfa nfa input, Hw.run_dfa dfa input)

(* run testcases *)
let _ =
  List.iter (fun (regex, str) ->
    let (b1,b2) = match_regex regex str in
    print_endline (string_of_bool b1 ^ ", " ^ string_of_bool b2)
  ) testcases

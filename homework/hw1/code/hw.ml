open Regex 

exception NotImplemented

let rec regex2nfa : Regex.t -> Nfa.t
= fun regex -> raise NotImplemented (* TODO *)

let nfa2dfa : Nfa.t -> Dfa.t
= fun nfa -> raise NotImplemented (* TODO *)

let run_nfa : Nfa.t -> symbol list -> bool
= fun nfa str -> raise NotImplemented (* TODO *)

let run_dfa : Dfa.t -> symbol list -> bool
= fun dfa str -> raise NotImplemented (* TODO *)

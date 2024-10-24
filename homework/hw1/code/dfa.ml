open Regex

type state = Nfa.states
type states = state BatSet.t
type delta = (state * symbol, state) BatMap.t
type init = state
type final = state BatSet.t

type t = states * delta * init * final

let mk_new_dfa : state -> t
= fun is ->
  (BatSet.singleton is, BatMap.empty, is, BatSet.empty)

let get_states : t -> states
= fun (states,_,_,_) -> states

let get_initial_state : t -> state
= fun (_,_,s,_) -> s

let get_next_state : t -> state -> symbol -> state
= fun (_,delta,_,_) s x ->
  try BatMap.find (s,x) delta
  with Not_found -> failwith "Dfa.get_next_state: Not found"

let is_final_state : t -> state -> bool
= fun (_,_,_,final) s -> BatSet.mem s final

let add_state : state -> t -> t
= fun s (states, delta, is, final) ->
  (BatSet.add s states, delta, is, final)

let add_final_state : state -> t -> t
= fun s (states, delta, init, final) ->
  (BatSet.add s states, delta, init, BatSet.add s final)

let add_edge : (state * symbol * state) -> t -> t
= fun (s,x,s') (states, delta, init, final) ->
  if not (BatSet.mem s states) || not (BatSet.mem s' states) 
    then failwith "Dfa.add_edge: states not found"
  else (states, BatMap.add (s,x) s' delta, init, final)

let string_of_state = Nfa.string_of_states

let string_of_states : states -> string
= fun states ->
  "{ " ^
    BatSet.fold (fun s acc ->
      if acc = "" then string_of_state s
      else acc ^ ", " ^ string_of_state s
    ) states "" ^
  " }"

let print : t -> unit
= fun (states, delta, is, final) ->
  print_endline ("* States: " ^ string_of_states states);
  print_endline ("* Initial State : " ^ string_of_state is); 
  print_endline ("* Final States : " ^ string_of_states final);
  prerr_endline "* Transition";
  BatMap.iter (fun (s,x) s' ->
    print_endline (string_of_state s ^ " --- " ^ string_of_symbol x ^ " ---> " ^ string_of_state s')
  ) delta

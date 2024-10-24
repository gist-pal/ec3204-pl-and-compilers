open Regex

(* a DFA state is a set of NFA states *)
type state = Nfa.states
type states = state BatSet.t

type t

(* create a new DFA with a start state *)
val mk_new_dfa : state -> t

(* return the set of all states of DFA *)
val get_states : t -> states

(* return the DFA's initial state *)
val get_initial_state : t -> state

(* get the next state that is directly reachable *)
val get_next_state : t -> state -> symbol -> state

(* check if a state is final *)
val is_final_state : t -> state -> bool

(* add a state to DFA *)
val add_state : state -> t -> t

(* add a state as a final state *)
val add_final_state : state -> t -> t

(* add a transition, i.e., update a transition function *)
val add_edge : (state * symbol * state) -> t -> t

(* string representation of a DFA state *)
val string_of_state : state -> string

(* string representation of DFA states *)
val string_of_states : states -> string

(* print DFA *)
val print : t -> unit

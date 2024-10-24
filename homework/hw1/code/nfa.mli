open Regex 

type state = int
type states = state BatSet.t
type delta = (state * symbol option, states) BatMap.t

type t

(* create a new NFA state *)
val mk_new_state : unit -> state

(* create a new NFA containing a single start state *)
val mk_new_nfa : unit -> t

(* return the set of all states of NFA *)
val get_states : t -> states

(* return the transition function *)
val get_delta : t -> delta

(* return the NFA's initial state *)
val get_initial_state : t -> state

(* return the set of final states *)
val get_final_states : t -> states

(* get the next states that are directly reachable *)
val get_next_states : t -> state -> symbol option -> states

(* check if a state is final *)
val is_final_state : t -> state -> bool

(* add a set of states into NFA *)
val add_states : states -> t -> t

(* add a state as a final state *)
val add_final_state : state -> t -> t

(* add a transition, i.e., update a transition function  *)
val add_edge : (state * symbol option * state) -> t -> t

(* add a transition function into NFA *)
val add_delta : delta -> t -> t

(* string representation of NFA states *)
val string_of_states : states -> string

(* print NFA *)
val print : t -> unit

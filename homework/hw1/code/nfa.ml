open Regex

type state = int
type states = state BatSet.t
type delta = (state * symbol option, states) BatMap.t
type init = state
type final = state BatSet.t

type t = states * delta * init * final
and nfa = t

let sid = ref 0

let mk_new_state : unit -> state
= fun () -> sid := !sid + 1; !sid

let mk_new_nfa : unit -> nfa
= fun () ->
  let is = mk_new_state () in
  (BatSet.singleton is, BatMap.empty, is, BatSet.empty)

let get_states : t -> states
= fun (states,_,_,_) -> states

let get_delta : t -> delta
= fun (_,delta,_,_) -> delta

let get_initial_state : t -> state
= fun (_,_,is,_) -> is

let get_final_states : t -> states
= fun (_,_,_,final) -> final

let get_next_states : t -> state -> symbol option -> states
= fun (_,delta,_,_) s x ->
  try BatMap.find (s,x) delta
  with Not_found -> BatSet.empty

let is_final_state : t -> state -> bool
= fun (_,_,_,final) s -> BatSet.mem s final

let add_states : states -> t -> t
= fun states' (states,delta,is,fs) ->
  (BatSet.union states states', delta, is, fs)

let add_final_state : state -> t -> t
= fun s (states, delta, init, final) ->
  (BatSet.add s states, delta, init, BatSet.add s final)

let add_edge : (state * symbol option * state) -> t -> t
= fun (s,x,s') ((states, delta, init, final) as nfa) ->
  if not (BatSet.mem s states) || not (BatSet.mem s' states)
    then failwith "Nfa.add_edge: states not found"
  else
    let nexts = BatSet.add s' (get_next_states nfa s x) in
    let delta' = BatMap.add (s,x) nexts delta in
    (states, delta', init, final)

let add_delta : delta -> t -> t
= fun delta' (states,delta,is,fs) ->
  (states, BatMap.union delta delta', is, fs)

let string_of_states : states -> string
= fun states ->
  "{" ^
    BatSet.fold (fun s acc ->
      if acc = "" then string_of_int s
      else acc ^ "," ^ string_of_int s
    ) states "" ^
  "}"

let string_of_symbol_op : symbol option -> string
= fun x ->
  match x with
  | None -> "epsilon"
  | Some x'-> string_of_symbol x'

let print : t -> unit
= fun (states, delta, is, final) ->
  print_endline ("* States: " ^ string_of_states states);
  print_endline ("* Initial State : " ^ string_of_int is);
  print_endline ("* Final States: " ^ string_of_states final);
  print_endline "* Transition";
  BatMap.iter (fun (s,x) nexts ->
    print_endline (string_of_int s ^ " ----- " ^ string_of_symbol_op x ^ " -----> " ^ string_of_states nexts)
  ) delta

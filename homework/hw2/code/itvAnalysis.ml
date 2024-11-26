open Vocab
open Lang
open AbsDom
open AbsDom.Itv

let rec eval_c : cmd -> AbsMem.t * int -> AbsMem.t * int
= fun cmd (m,proven) -> raise NotImplemented (* TODO *)

let init_param m (typ,x) =
  match typ with
  | T_Int -> AbsMem.add x Itv.top m
  | T_Array n ->
    m |> AbsMem.add x Itv.top
      |> AbsMem.add ("len_" ^ x) (Itv.alpha n)

let run : pgm -> int
= fun (fid,params,cmd) ->
  let m0 = List.fold_left init_param AbsMem.empty params in
  print_endline "[INFO] initial memory state";
  AbsMem.print m0;
  snd (eval_c cmd (m0,0))

let main () =
  let print_code = ref false in
  let src = ref "" in
  let spec = [("-pp", Arg.Set print_code, "pretty print the input program")] in
  let usage = "Usage: run <options> <file>" in
  let _ = Arg.parse spec
              (fun
                 x ->
                   if Sys.file_exists x then src := x
                   else raise (Arg.Bad (x ^ ": No files given")))
              usage
  in

  if !src = "" then Arg.usage spec usage
  else
  try
    let file_channel = open_in !src in
    let lexbuf = Lexing.from_channel file_channel in
    let s_pgm = Parser.program Lexer.start lexbuf in
    let _ = print_endline "== source program ==" in
    let _ = S.pp s_pgm in
    let _ = print_endline "== execution result (source) ==" in
    let _ = try S.execute s_pgm with (Failure s) -> print_endline ("Error: "^s) in

    let t_pgm = Translator.translate s_pgm in
    let _ = print_endline "== translated target program ==" in
    let _ = T.pp t_pgm in
    let _ = print_endline "== execution result (translated) ==" in
    let _ = try T.execute t_pgm with (Failure s) -> print_endline ("Error: "^s) in
    let t_pgm_opt = Optimizer.optimize t_pgm in
    let _ = print_endline "== optimized target program ==" in
    let _ = T.pp t_pgm_opt in
    let _ = print_endline "== execution result (optimized) ==" in
    try T.execute t_pgm_opt with (Failure s) -> print_endline ("Error: "^s)

  with (Failure s) -> print_endline (!src ^ ": " ^ s)

let _ = main ()

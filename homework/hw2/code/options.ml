let inputfile = ref ""

let options =
  [
    ("-input", (Arg.String (fun s -> inputfile := s)), "inputfile containing your examples");
  ]

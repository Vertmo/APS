let usage = "usage: " ^ Sys.argv.(0) ^ " [-parse] [-type] [-eval] <input_file>"

type step = Parse | Type | Eval

let step = ref Eval

let speclist = [
  ("-parse", Arg.Unit (fun () -> step := Parse), ": only parse the program");
  ("-type", Arg.Unit (fun () -> step := Type), ": verify typing of the program");
  ("-eval", Arg.Unit (fun () -> step := Eval), ": evaluate the program (default)")
]

let lex_and_parse ic =
  let lexbuf = Lexing.from_channel ic in
  Parser.prog Lexer.token lexbuf

let main filename step =
  let ic = open_in filename in
  let prog = lex_and_parse ic in
  if(step = Parse) then (print_endline (Ast.string_of_prog prog))
  else (
    Typer.type_check prog;
    if(step = Type) then (print_endline "Type checking OK !")
    else ()
  )

let _ = Arg.parse speclist (fun x -> main x !step) usage

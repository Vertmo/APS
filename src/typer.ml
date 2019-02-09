open Ast

exception TypeError

let rec prolog_of_type = function
  | Int -> "int" | Bool -> "bool" | Void -> "void"
  | Fun (l, t) -> Printf.sprintf "[%s,%s]" (String.concat "," (List.map prolog_of_type l)) (prolog_of_type t)

let prolog_of_args a = (String.concat "," (List.map (fun (s, t) -> Printf.sprintf "\"%s\":%s" s (prolog_of_type t)) a))

let rec prolog_of_expr = function
  | Bool b -> if b then "sym(\"true\")" else "sym(\"false\")"
  | IntConst i -> string_of_int i
  | Sym s -> Printf.sprintf "sym(\"%s\")" s
  | If (c, t, e) -> Printf.sprintf "if(%s, %s, %s)" (prolog_of_expr c) (prolog_of_expr t) (prolog_of_expr e)
  | Op (op, exprs) -> Printf.sprintf "app(sym(\"%s\"), [%s])" (string_of_opprim op)
                        (String.concat "," (List.map prolog_of_expr exprs))
  | Abs (args, body) -> Printf.sprintf "abs([%s], %s)" (prolog_of_args args) (prolog_of_expr body)
  | App (f, exprs) -> Printf.sprintf "app(%s, [%s])" (prolog_of_expr f) (String.concat "," (List.map prolog_of_expr exprs))

let prolog_of_stat = function
  | Echo e -> Printf.sprintf "echo(%s)" (prolog_of_expr e)
  | _ -> failwith "Stat not yet implemented"

let prolog_of_dec = function
  | ConstDec (x, t, e) -> Printf.sprintf "const(\"%s\", %s, %s)" x (prolog_of_type t) (prolog_of_expr e)
  | FunDec (x, t, a, e) -> Printf.sprintf "fun(\"%s\", %s, [%s], %s)"
                             x
                             (prolog_of_type t)
                             (prolog_of_args a)
                             (prolog_of_expr e)
  | RecFunDec (x, t, a, e) -> Printf.sprintf "funrec(\"%s\", %s, [%s], %s)"
                                x
                                (prolog_of_type t)
                                (prolog_of_args a)
                                (prolog_of_expr e)
  | _ -> failwith "Dec not yet implemented"

let prolog_of_cmd = function
  | Stat s -> prolog_of_stat s
  | Dec d -> prolog_of_dec d

let prolog_of_prog p = Printf.sprintf "[%s]"
    (String.concat "," (List.map prolog_of_cmd p))


let type_check p =
  let ps = prolog_of_prog p in
  (* print_endline ps; *)
  if (Sys.command
            (Printf.sprintf
               "gprolog --consult-file ./src/typer.pl --query-goal 'check_type(%s)' --query-goal 'halt' | grep yes > /dev/null"
               ps)) <> 0 then raise TypeError

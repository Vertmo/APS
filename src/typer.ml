open Ast

exception TypeError

let rec prolog_of_type = function
  | Int -> "int" | Bool -> "bool" | Void -> "void"
  | Vec t -> Printf.sprintf "vec(%s)" (prolog_of_type t)
  | Fun (l, t) -> Printf.sprintf "tfun([%s],%s)" (String.concat "," (List.map prolog_of_type l)) (prolog_of_type t)
  | TypeVar s -> Printf.sprintf "tvar(\"%s\")" s
  | Product (t1, t2) -> Printf.sprintf "tprod(%s, %s)" (prolog_of_type t1) (prolog_of_type t2)

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
  | Let (x, e, b) -> Printf.sprintf "let(\"%s\", %s, %s)" x (prolog_of_expr e) (prolog_of_expr b)
  | Pair (e1, e2) -> Printf.sprintf "pair(%s, %s)" (prolog_of_expr e1) (prolog_of_expr e2)
  | Fst e -> Printf.sprintf "fst(%s)" (prolog_of_expr e) | Snd e -> Printf.sprintf "snd(%s)" (prolog_of_expr e)

and prolog_of_dec = function
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
  | VarDec (x, t) -> Printf.sprintf "var(\"%s\", %s)" x (prolog_of_type t)
  | ProcDec (x, a, p) -> Printf.sprintf "proc(\"%s\", [%s], %s)" x (prolog_of_args a) (prolog_of_prog p)
  | RecProcDec (x, a, p) -> Printf.sprintf "procrec(\"%s\", [%s], %s)" x (prolog_of_args a) (prolog_of_prog p)
  | FunProcDec (x, t, a, b) -> Printf.sprintf "funproc(\"%s\", %s, [%s], %s)"
                                 x
                                 (prolog_of_type t)
                                 (prolog_of_args a)
                                 (prolog_of_prog b)
  | RecFunProcDec (x, t, a, b) -> Printf.sprintf "funprocrec(\"%s\", %s, [%s], %s)"
                                x
                                (prolog_of_type t)
                                (prolog_of_args a)
                                (prolog_of_prog b)

and prolog_of_lval = function
  | SymLval s -> Printf.sprintf "sym(\"%s\")" s
  | Nth (l, e) -> Printf.sprintf "app(sym(\"nth\"), [%s,%s])" (prolog_of_lval l) (prolog_of_expr e)

and prolog_of_stat = function
  | Echo e -> Printf.sprintf "echo(%s)" (prolog_of_expr e)
  | Set (x, e) -> Printf.sprintf "set(%s, %s)" (prolog_of_lval x) (prolog_of_expr e)
  | Ifs (c, t, e) -> Printf.sprintf "ifs(%s, %s, %s)" (prolog_of_expr c) (prolog_of_prog t) (prolog_of_prog e)
  | While (c, b) -> Printf.sprintf "while(%s, %s)" (prolog_of_expr c) (prolog_of_prog b)
  | Call (x, exprs) -> Printf.sprintf "call(\"%s\", [%s])" x (String.concat "," (List.map prolog_of_expr exprs))

and prolog_of_ret (Return e) = Printf.sprintf "return(%s)" (prolog_of_expr e)

and prolog_of_cmd = function
  | Stat s -> prolog_of_stat s
  | Dec d -> prolog_of_dec d
  | Ret r -> prolog_of_ret r

and prolog_of_prog p = Printf.sprintf "[%s]"
    (String.concat "," (List.map prolog_of_cmd p))


let type_check p print_prog =
  let ps = prolog_of_prog p in
  if print_prog then (print_endline ps);
  if (Sys.command
            (Printf.sprintf
               "gprolog --consult-file ./src/typer.pl --query-goal 'check_type(%s)' --query-goal 'halt' | grep yes > /dev/null"
               ps)) <> 0 then raise TypeError

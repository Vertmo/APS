type eType = Int | Bool | Void | Fun of (eType list * eType)

let rec string_of_type = function
  | Int -> "int" | Bool -> "bool" | Void -> "void"
  | Fun (l, t) -> Printf.sprintf "(%s -> %s)" (String.concat "*" (List.map string_of_type l)) (string_of_type t)

type opPrim = Not | And | Or | Eq | Lt | Add | Sub | Mul | Div

let string_of_opprim = function | Not -> "not" | And -> "and" | Or -> "or"
                                | Eq -> "eq" | Lt -> "lt"
                                | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div"

type arg = (string * eType)

let string_of_args a = (String.concat "," (List.map (fun (s, t) -> Printf.sprintf "%s:%s" s (string_of_type t)) a))

type expr =
  | Bool of bool
  | IntConst of int
  | Sym of string
  | If of expr * expr * expr
  | Op of opPrim * expr list
  | Abs of arg list * expr
  | App of expr * expr list

and stat =
  | Echo of expr
  | Set of string * expr
  | Ifs of expr * prog * prog
  | While of expr * prog
  | Call of string * expr list

and dec =
  | ConstDec of string * eType * expr
  | FunDec of string * eType * arg list * expr
  | RecFunDec of string * eType * arg list * expr
  | VarDec of string * eType
  | ProcDec of string * arg list * prog
  | RecProcDec of string * arg list * prog

and cmd = Stat of stat | Dec of dec

and prog = cmd list

let rec string_of_expr = function
  | Bool b -> if b then "true" else "false"
  | IntConst i -> string_of_int i
  | Sym s -> s
  | If (c, t, e) -> Printf.sprintf "(if %s %s %s)" (string_of_expr c) (string_of_expr t) (string_of_expr e)
  | Op (op, exprs) -> Printf.sprintf "(%s %s)" (string_of_opprim op) (String.concat " " (List.map string_of_expr exprs))
  | Abs (args, body) -> Printf.sprintf ("[%s] %s") (string_of_args args) (string_of_expr body)
  | App (f, exprs) -> Printf.sprintf "(%s %s)" (string_of_expr f) (String.concat " " (List.map string_of_expr exprs))

and string_of_stat = function
  | Echo e -> Printf.sprintf "ECHO %s" (string_of_expr e)
  | Set (x, e) -> Printf.sprintf "SET %s %s" x (string_of_expr e)
  | Ifs (c, t, e) -> Printf.sprintf "IF %s\n%s\n%s" (string_of_expr c) (string_of_prog t) (string_of_prog e)
  | While (c, b) -> Printf.sprintf "WHILE %s\n%s" (string_of_expr c) (string_of_prog b)
  | Call (p, exprs) -> Printf.sprintf "CALL %s %s" p (String.concat " " (List.map string_of_expr exprs))

and string_of_dec = function
  | ConstDec (x, t, e) -> Printf.sprintf "CONST %s %s %s" x (string_of_type t) (string_of_expr e)
  | FunDec (x, t, a, e) -> Printf.sprintf "FUN %s %s [%s] %s"
                             x
                             (string_of_type t)
                             (string_of_args a)
                             (string_of_expr e)
  | RecFunDec (x, t, a, e) -> Printf.sprintf "FUN REC %s %s [%s] %s"
                                x
                                (string_of_type t)
                                (string_of_args a)
                                (string_of_expr e)
  | VarDec (x, t) -> Printf.sprintf "VAR %s %s" x (string_of_type t)
  | ProcDec (x, a, p) -> Printf.sprintf "PROC %s [%s]\n%s" x (string_of_args a) (string_of_prog p)
  | RecProcDec (x, a, p) -> Printf.sprintf "PROC REC %s [%s]\n%s" x (string_of_args a) (string_of_prog p)

and string_of_cmd = function
  | Stat s -> string_of_stat s
  | Dec d -> string_of_dec d

and string_of_prog p = Printf.sprintf "[%s]" (String.concat ";\n" (List.map string_of_cmd p))

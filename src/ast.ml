type eType = Int | Bool | Void | Unit
           | Vec of eType
           | Fun of (eType list * eType)
           | TypeVar of string
           | Product of (eType * eType)
           | Sum of (eType * eType)

let rec string_of_type = function
  | Int -> "int" | Bool -> "bool" | Void -> "void"
  | Vec t -> Printf.sprintf "vec %s" (string_of_type t)
  | Fun (l, t) -> Printf.sprintf "(%s -> %s)" (String.concat "*" (List.map string_of_type l)) (string_of_type t)
  | TypeVar s -> s
  | Product (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_type t1) (string_of_type t2)
  | Sum (t1, t2) -> Printf.sprintf "(%s + %s)" (string_of_type t1) (string_of_type t2)
  | Unit -> "unit"

type opPrim = Not | And | Or | Eq | Lt | Add | Sub | Mul | Div | Len | Nth | Alloc

let string_of_opprim = function | Not -> "not" | And -> "and" | Or -> "or"
                                | Eq -> "eq" | Lt -> "lt"
                                | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div"
                                | Len -> "len" | Nth -> "nth" | Alloc -> "alloc"

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
  | Let of string * expr * expr
  | Pair of expr * expr | Fst of expr | Snd of expr
  | InL of eType * expr | InR of eType * expr
  | Case of expr * string * expr * string * expr
  | Unit

and dec =
  | ConstDec of string * eType * expr
  | FunDec of string * eType * arg list * expr
  | RecFunDec of string * eType * arg list * expr
  | VarDec of string * eType
  | ProcDec of string * arg list * prog
  | RecProcDec of string * arg list * prog
  | FunProcDec of string * eType * arg list * prog
  | RecFunProcDec of string * eType * arg list * prog

and lval = SymLval of string | Nth of lval * expr

and stat =
  | Echo of expr
  | Set of lval * expr
  | Ifs of expr * prog * prog
  | While of expr * prog
  | Call of string * expr list

and ret = Return of expr

and cmd = Dec of dec | Stat of stat | Ret of ret

and prog = cmd list

let rec string_of_expr = function
  | Bool b -> if b then "true" else "false"
  | IntConst i -> string_of_int i
  | Sym s -> s
  | If (c, t, e) -> Printf.sprintf "(if %s %s %s)" (string_of_expr c) (string_of_expr t) (string_of_expr e)
  | Op (op, exprs) -> Printf.sprintf "(%s %s)" (string_of_opprim op) (String.concat " " (List.map string_of_expr exprs))
  | Abs (args, body) -> Printf.sprintf ("[%s] %s") (string_of_args args) (string_of_expr body)
  | App (f, exprs) -> Printf.sprintf "(%s %s)" (string_of_expr f) (String.concat " " (List.map string_of_expr exprs))
  | Let (x, e, b) -> Printf.sprintf "let %s = %s in %s" x (string_of_expr e) (string_of_expr b)
  | Pair (e1, e2) -> Printf.sprintf "(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Fst e -> Printf.sprintf "%s.fst" (string_of_expr e) | Snd e -> Printf.sprintf "%s.fst" (string_of_expr e)
  | InL (t, e) -> Printf.sprintf "(inl %s %s)" (string_of_type t) (string_of_expr e)
  | InR (t, e) -> Printf.sprintf "(inr %s %s)" (string_of_type t) (string_of_expr e)
  | Case (e, s1, e1, s2, e2) -> Printf.sprintf "(case %s of\n %s => %s\n | %s => %s)"
                                  (string_of_expr e) s1 (string_of_expr e1) s2 (string_of_expr e2)
  | Unit -> "()"

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
  | FunProcDec (x, t, a, p) -> Printf.sprintf "FUN %s %s [%s]\n%s"
                                 x (string_of_type t) (string_of_args a) (string_of_prog p)
  | RecFunProcDec (x, t, a, p) -> Printf.sprintf "REC FUN %s %s [%s]\n%s"
                                 x (string_of_type t) (string_of_args a) (string_of_prog p)

and string_of_lval = function
  | SymLval s -> s
  | Nth (l, e) -> Printf.sprintf "(nth %s %s)" (string_of_lval l) (string_of_expr e)

and string_of_stat = function
  | Echo e -> Printf.sprintf "ECHO %s" (string_of_expr e)
  | Set (x, e) -> Printf.sprintf "SET %s %s" (string_of_lval x) (string_of_expr e)
  | Ifs (c, t, e) -> Printf.sprintf "IF %s\n%s\n%s" (string_of_expr c) (string_of_prog t) (string_of_prog e)
  | While (c, b) -> Printf.sprintf "WHILE %s\n%s" (string_of_expr c) (string_of_prog b)
  | Call (p, exprs) -> Printf.sprintf "CALL %s %s" p (String.concat " " (List.map string_of_expr exprs))

and string_of_ret (Return e) = Printf.sprintf "RETURN %s" (string_of_expr e)

and string_of_cmd = function
  | Stat s -> string_of_stat s
  | Dec d -> string_of_dec d
  | Ret r -> string_of_ret r

and string_of_prog p = Printf.sprintf "[%s]" (String.concat ";\n" (List.map string_of_cmd p))

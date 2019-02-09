open Ast

type value = IntVal of int | Closure of closure | RecClosure of recclosure
and closure = (expr * (value list -> env))
and recclosure = value -> closure
and env = (string * value) list
type outFlow = int list
type context = env * outFlow

let int_of_ival = function IntVal i -> i | _ -> failwith "Should not happen"

let rec lookup_env env s = match env with
  | (s', v)::_ when s = s' -> v
  | _::q -> lookup_env q s
  | [] -> failwith "Should not happen"

let eval_op op vals = match op with
  | Not -> if (int_of_ival (List.hd vals)) = 0 then IntVal 1 else IntVal 0
  | And -> if (int_of_ival (List.hd vals)) = 0 then IntVal 0 else (List.hd (List.tl vals))
  | Or -> if (int_of_ival (List.hd vals)) = 1 then IntVal 1 else (List.hd (List.tl vals))
  | Eq -> if (int_of_ival (List.hd vals)) = (int_of_ival (List.hd (List.tl vals))) then IntVal 1 else IntVal 0
  | Lt -> if (int_of_ival (List.hd vals)) < (int_of_ival (List.hd (List.tl vals))) then IntVal 1 else IntVal 0
  | Add -> IntVal ((int_of_ival (List.hd vals)) + (int_of_ival (List.hd (List.tl vals))))
  | Sub -> IntVal ((int_of_ival (List.hd vals)) - (int_of_ival (List.hd (List.tl vals))))
  | Mul -> IntVal ((int_of_ival (List.hd vals)) * (int_of_ival (List.hd (List.tl vals))))
  | Div -> IntVal ((int_of_ival (List.hd vals)) / (int_of_ival (List.hd (List.tl vals))))

(** Evaluate an expression and return a value *)
let rec eval_expr env e = match e with
  | Bool b -> if b then IntVal 1 else IntVal 0
  | IntConst i -> IntVal i
  | Sym s -> lookup_env env s
  | If (c, t, e) -> (match (eval_expr env c) with
      | IntVal 1 -> (eval_expr env t)
      | IntVal 0 -> (eval_expr env e)
      | _ -> failwith "Should not happen")
  | Op (op, exprs) -> eval_op op (List.map (eval_expr env) exprs)
  | App (f, exprs) -> let c = eval_expr env f and args = (List.map (eval_expr env) exprs) in
    (match c with
    | Closure (e', r) -> eval_expr (r args) e'
    | RecClosure rf -> let (e', r) = (rf c) in eval_expr (r args) e'
    | _ -> failwith "Should not happen")
  | Abs (a, e) -> Closure (e, (fun args -> (List.combine (fst (List.split a)) args)@env))

(** Evaluate a declaration *)
and eval_dec env = function
  | ConstDec (x, _, e) -> let v = eval_expr env e in (x, v)::env
  | FunDec (x, _, a, e) ->
    let c = Closure (e, fun args -> (List.combine (fst (List.split a)) args)@env)
    in (x, c)::env
  | RecFunDec (x, _, a, e) ->
    let rc = RecClosure (fun f ->
        (e, fun args -> (x, f)::(List.combine (fst (List.split a)) args)@env)) in
    (x, rc)::env
  | _ -> failwith "Dec not yet implemented"

(** Evaluate a statement *)
and eval_stat env flow = function
  | Echo e -> (match (eval_expr env e) with IntVal i -> (i::flow)
                                          | _ -> failwith "Should not happen")
  | _ -> failwith "Stat not yet implemented"

(** Evaluate a single command (either declaration or statement) *)
and eval_cmd (env, flow) = function
  | Stat s -> (env, eval_stat env flow s)
  | Dec d -> (eval_dec env d, flow)

(** Evaluate a program *)
and eval_prog p =
  let (_, outFlow) = List.fold_left eval_cmd ([], []) p in
  List.rev outFlow

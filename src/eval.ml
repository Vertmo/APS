open Ast

type value = IntVal of int | Closure of closure | RecClosure of recclosure
           | Addr of int | ProcClosure of procclosure | RecProcClosure of recprocclosure
and closure = (expr * (value list -> env))
and recclosure = value -> closure
and procclosure = (cmd list * (value list -> env))
and recprocclosure = value -> procclosure
and env = (string * value) list

type 'a memspace = None | Any | Some of 'a
type mem = int memspace array

type outFlow = int list

let int_of_ival = function IntVal i -> i | _ -> failwith "Should not happen"

let rec lookup_env env s = match env with
  | (s', v)::_ when s = s' -> v
  | _::q -> lookup_env q s
  | [] -> failwith "Should not happen"

(* On utilise des tableaux dynamiques pour gérer la mémoire *)
let alloc m =
  if(Array.memq None m) then ( (* Il y a de la place *)
    let i = ref 0 and changeDone = ref false in
    while(!i < (Array.length m) && not !changeDone) do
      if(m.(!i) = None) then (
        m.(!i) <- Any;
        changeDone := true;
      )
      else (i := !i+1)
    done;
    (!i, m)
  ) else (
    let n = Array.length m in
    let m' = Array.make (n*2) None in
    Array.blit m 0 m' 0 n;
    m'.(n) <- Any;
    (n, m')
  )

let restrict_mem mem env =
  for i = 0 to (Array.length mem)-1 do
    if (mem.(i) <> None && not (List.exists (fun (_, v) -> (match v with
        | Addr a when a = i -> true
        | _ -> false)) env))
    then mem.(i) <- None
  done; mem

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
let rec eval_expr (env, mem) e = match e with
  | Bool b -> if b then IntVal 1 else IntVal 0
  | IntConst i -> IntVal i
  | Sym s -> (match (lookup_env env s) with
      | Addr a -> (match (mem.(a)) with
          | Some v -> v
          | Any -> IntVal 0 (* On décide qu'en cas de variable non initialisé, la valeur par défaut est 0 *)
          | _ -> failwith (Printf.sprintf "%s is not in memory" s))
      | v -> v)
  | If (c, t, e) -> (match (eval_expr (env, mem) c) with
      | IntVal 1 -> (eval_expr (env, mem) t)
      | IntVal 0 -> (eval_expr (env, mem) e)
      | _ -> failwith "Should not happen")
  | Op (op, exprs) -> eval_op op (List.map (eval_expr (env, mem)) exprs)
  | App (f, exprs) -> let c = eval_expr (env, mem) f and args = (List.map (eval_expr (env, mem)) exprs) in
    (match c with
    | Closure (e', r) -> eval_expr ((r args), mem) e'
    | RecClosure rf -> let (e', r) = (rf c) in eval_expr ((r args), mem) e'
    | _ -> failwith "Should not happen")
  | Abs (a, e) -> Closure (e, (fun args -> (List.combine (fst (List.split a)) args)@env))

(** Evaluate a declaration *)
and eval_dec (env, mem) = function
  | ConstDec (x, _, e) -> let v = eval_expr (env, mem) e in ((x, v)::env, mem)
  | FunDec (x, _, a, e) ->
    let c = Closure (e, fun args -> (List.combine (fst (List.split a)) args)@env)
    in ((x, c)::env, mem)
  | RecFunDec (x, _, a, e) ->
    let rc = RecClosure (fun f ->
        (e, fun args -> (x, f)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rc)::env, mem)
  | VarDec (x, _) -> let (a, mem') = alloc mem in ((x, Addr a)::env, mem')
  | ProcDec (x, a, b) -> let pc = ProcClosure (b, fun args -> (List.combine (fst (List.split a)) args)@env) in
    ((x, pc)::env, mem)
  | RecProcDec (x, a, b) -> let rpc = RecProcClosure (fun p ->
      (b, fun args -> (x, p)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rpc)::env, mem)

(** Evaluate a statement *)
and eval_stat (env, mem, outFlow) = function
  | Echo e -> let outFlow = (match (eval_expr (env, mem) e) with IntVal i -> (i::outFlow)
                                                               | _ -> failwith "Should not happen") in
    (mem, outFlow)
  | Set (s, e) -> (match (lookup_env env s) with
      | Addr a -> let v = eval_expr (env, mem) e in
        mem.(a) <- (Some v); (mem, outFlow)
      | _ -> failwith (Printf.sprintf "%s is not a variable" s))
  | Ifs (c, t, e) -> (match (eval_expr (env, mem) c) with
      | IntVal 1 -> let (mem, outFlow) = eval_block (env, mem, outFlow) t in
        ((restrict_mem mem env), outFlow)
      | IntVal 0 -> let (mem, outFlow) = eval_block (env, mem, outFlow) e in
        ((restrict_mem mem env), outFlow)
      | _ -> failwith "Should not happen")
  | While (c, b) -> (match (eval_expr (env, mem) c) with
      | IntVal 0 -> (mem, outFlow)
      | IntVal 1 -> let (mem', outFlow') = eval_block (env, mem, outFlow) b in
        eval_stat (env, (restrict_mem mem' env), outFlow') (While (c, b))
      | _ -> failwith "Should not happen")
  | Call (x, exprs) -> let p = lookup_env env x and args = (List.map (eval_expr (env, mem)) exprs) in
    (match p with
     | ProcClosure (b, r) -> let (mem', outFlow') = eval_block ((r args), mem, outFlow) b in
       ((restrict_mem mem' env), outFlow')
     | RecProcClosure rp -> let (b, r) = rp p in
       let (mem', outFlow') = eval_block ((r args), mem, outFlow) b in
       ((restrict_mem mem' env), outFlow')
     | _ -> failwith "Should not happen")

(** Evaluate a single command (either declaration or statement) *)
and eval_cmd (env, mem, outFlow) = function
  | Dec d -> let (env, mem) = eval_dec (env, mem) d in (env, mem, outFlow)
  | Stat s -> let (mem, outFlow) = eval_stat (env, mem, outFlow) s in (env, mem, outFlow)

(** Evaluate a program *)
and eval_block startState p =
  let (_, mem, outFlow) = List.fold_left eval_cmd startState p in
  (mem, outFlow)

let eval_prog p =
  let (_, outFlow) = eval_block ([], [|None|], []) p in
  List.rev outFlow

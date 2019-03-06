open Ast

type value = IntVal of int | Closure of closure | RecClosure of recclosure
           | Addr of int | ProcClosure of procclosure | RecProcClosure of recprocclosure
           | Block of int * int
and closure = (expr * (value list -> env))
and recclosure = value -> closure
and procclosure = (cmd list * (value list -> env))
and recprocclosure = value -> procclosure
and env = (string * value) list
and valueE = Empty | Value of value

type 'a memspace = None | Any | Some of 'a
type mem = value memspace array

type outFlow = int list

exception ArrayIndexOutOfBoundsEx

let int_of_ival = function IntVal i -> i | _ -> failwith "int_of_ival: Should not happen"

let rec lookup_env env s = match env with
  | (s', v)::_ when s = s' -> v
  | _::q -> lookup_env q s
  | [] -> failwith "lookup_env: Should not happen"

(* On utilise des tableaux dynamiques pour gérer la mémoire *)
let rec allocn m n =
  let created = ref (None:int option) in
  for i = 0 to (Array.length m) - n do
    if !created = None then (
      let isSpace = ref true in
      for j = i to i+n-1 do
        if m.(j) <> None then isSpace := false;
      done;
      if !isSpace then created := Some i;
    )
  done;
  match !created with
  | Some k -> (
      for i = k to k+n-1 do
        m.(i) <- Any
      done;
      (k, m)
    )
  | None -> (
      let l = Array.length m in
      let m' = Array.make (l*2) None in
      Array.blit m 0 m' 0 l;
      allocn m' n
    )

let compute_all_reachable mem env =
  let rec c_a_reach_aux env =
    if (env = []) then []
    else (
      let reachables = List.fold_left (fun l v -> match v with
          | Addr i -> i::l
          | Block (i, n) -> (List.init n (fun j -> i+j))@l
          | _ -> l) [] env in
      reachables@(c_a_reach_aux (List.fold_left (fun l r -> match mem.(r) with
          | Some (Block (i, n)) -> (Block (i, n))::l
          | _ -> l) [] reachables))) in
  c_a_reach_aux (List.map (fun (_, v) -> v) env)

let restrict_mem mem (* env *) _ =
  (* let reachables = compute_all_reachable mem env in
   * for i = 0 to (Array.length mem)-1 do
   *   if (mem.(i) <> None && not (List.mem i reachables))
   *   then mem.(i) <- None
   * done; *) mem

let eval_op op vals mem = match op with
  | Not -> if (int_of_ival (List.hd vals)) = 0 then IntVal 1, mem else IntVal 0, mem
  | And -> if (int_of_ival (List.hd vals)) = 0 then IntVal 0, mem else (List.hd (List.tl vals)), mem
  | Or -> if (int_of_ival (List.hd vals)) = 1 then IntVal 1, mem else (List.hd (List.tl vals)), mem
  | Eq -> if (int_of_ival (List.hd vals)) = (int_of_ival (List.hd (List.tl vals))) then IntVal 1, mem else IntVal 0, mem
  | Lt -> if (int_of_ival (List.hd vals)) < (int_of_ival (List.hd (List.tl vals))) then IntVal 1, mem else IntVal 0, mem
  | Add -> IntVal ((int_of_ival (List.hd vals)) + (int_of_ival (List.hd (List.tl vals)))), mem
  | Sub -> IntVal ((int_of_ival (List.hd vals)) - (int_of_ival (List.hd (List.tl vals)))), mem
  | Mul -> IntVal ((int_of_ival (List.hd vals)) * (int_of_ival (List.hd (List.tl vals)))), mem
  | Div -> IntVal ((int_of_ival (List.hd vals)) / (int_of_ival (List.hd (List.tl vals)))), mem
  | Len -> (match (List.hd vals) with
      | Block (_, n) -> IntVal n, mem | _ -> failwith "Len: Should not happen")
  | Nth -> (match ((List.hd vals), (List.hd (List.tl vals))) with
      | (Block (a, n), IntVal i) -> if i >= n || i < 0 then raise ArrayIndexOutOfBoundsEx;
        (match mem.(a+i) with
         | Some v -> v, mem
         | Any -> IntVal 0, mem
         | None -> failwith "Not in memory")
      | _ -> failwith "Nth: Should not happen")
  | Alloc -> (match (List.hd vals) with
      | IntVal n -> let (a, mem') = allocn mem n in (Block (a, n), mem')
      | _ -> failwith "Alloc: Should not happen")

(** Evaluate an expression and return a value *)
let rec eval_expr (env, mem, outFlow) = function
  | Bool b -> if b then IntVal 1, mem, outFlow else IntVal 0, mem, outFlow
  | IntConst i -> IntVal i, mem, outFlow
  | Sym s -> (match (lookup_env env s) with
      | Addr a -> (match (mem.(a)) with
          | Some v -> v, mem, outFlow
          | Any -> IntVal 0, mem, outFlow (* On décide qu'en cas de variable non initialisé, la valeur par défaut est 0 *)
          | _ -> failwith (Printf.sprintf "%s is not in memory (addr %d is empty)" s a))
      | v -> v, mem, outFlow)
  | If (c, t, e) -> (match (eval_expr (env, mem, outFlow) c) with
      | IntVal 1, mem', outFlow' -> (eval_expr (env, mem', outFlow') t)
      | IntVal 0, mem', outFlow' -> (eval_expr (env, mem', outFlow') e)
      | _ -> failwith "If: Should not happen")
  | Op (op, exprs) ->
    let (args, mem, outFlow') = (List.fold_left (fun (a, m, outF) e ->
        let (v, m', outF') = eval_expr (env, m, outF) e in v::a, m', outF') ([], mem, outFlow) exprs) in
    let (v, m'') = eval_op op (List.rev args) mem in (v, m'', outFlow')
  | App (f, exprs) -> let (c, mem', outFlow') = eval_expr (env, mem, outFlow) f in
    let (args, mem'', outFlow'') = (List.fold_left (fun (a, m, outF) e ->
        let (v, m', outF') = eval_expr (env, m, outF) e in v::a, m', outF') ([], mem', outFlow') exprs) in
    (match c with
     | Closure (e', r) -> eval_expr ((r (List.rev args)), mem'', outFlow'') e'
     | RecClosure rf -> let (e', r) = (rf c) in eval_expr ((r (List.rev args)), mem'', outFlow'') e'
     | ProcClosure (b, r) -> let (v, mem''', outFlow''') = eval_block ((r (List.rev args)), mem'', outFlow'') b in
       (match v with
        | Value v -> (v, restrict_mem mem''' (("'d", v)::env), outFlow''')
        | Empty -> failwith "App(ProcClosure): Should not happen"
       )
     | RecProcClosure rp -> let (b, r) = (rp c) in
       let (v, mem''', outFlow''') = eval_block ((r (List.rev args)), mem'', outFlow'') b in
       (match v with
        | Value v -> (v, restrict_mem mem''' (("'d", v)::env), outFlow''')
        | Empty -> failwith "App(ProcClosure): Should not happen"
       )
     | _ -> failwith "App: Should not happen")
  | Abs (a, e) -> Closure (e, (fun args -> (List.combine (fst (List.split a)) args)@env)), mem, outFlow
  | Let (x, e, b) -> let (v, mem', outFlow') = eval_expr (env, mem, outFlow) e in
    eval_expr ((x,v)::env, mem', outFlow') b

(** Evaluate a declaration *)
and eval_dec (env, mem, outFlow) = function
  | ConstDec (x, _, e) ->
    let (v, mem', outFlow') = eval_expr (env, mem, outFlow) e in ((x, v)::env, mem', outFlow')
  | FunDec (x, _, a, e) ->
    let c = Closure (e, fun args -> (List.combine (fst (List.split a)) args)@env)
    in ((x, c)::env, mem, outFlow)
  | RecFunDec (x, _, a, e) ->
    let rc = RecClosure (fun f ->
        (e, fun args -> (x, f)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rc)::env, mem, outFlow)
  | VarDec (x, _) -> let (a, mem') = allocn mem 1 in ((x, Addr a)::env, mem', outFlow)
  | ProcDec (x, a, b) -> let pc = ProcClosure (b, fun args -> (List.combine (fst (List.split a)) args)@env) in
    ((x, pc)::env, mem, outFlow)
  | RecProcDec (x, a, b) -> let rpc = RecProcClosure (fun p ->
      (b, fun args -> (x, p)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rpc)::env, mem, outFlow)
  | FunProcDec (x, _, a, b) ->
    let pc = ProcClosure (b, fun args -> (List.combine (fst (List.split a)) args)@env) in
    ((x, pc)::env, mem, outFlow)
  | RecFunProcDec (x, _, a, b) ->
    let rpc = RecProcClosure (fun p ->
        (b, fun args -> (x, p)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rpc)::env, mem, outFlow)

(** Evaluate an left value *)
and eval_lval (env, mem, outFlow) = function
  | SymLval s -> (match (lookup_env env s) with
      | Addr a -> ((Addr a), mem, outFlow)
      | Block (b, n) -> (Block (b,n), mem, outFlow)
      | _ -> failwith "SymLval: Should not happen")
  | Nth (lval, ie) -> let (b, mem', outFlow') = eval_lval (env, mem, outFlow) lval in
    let (iv, mem'', outFlow'') = eval_expr (env, mem', outFlow') ie in (match b, iv with
      | Block (b, n), IntVal i -> if i < 0 || i > n then raise ArrayIndexOutOfBoundsEx;
        (Addr (b+i), mem'', outFlow'')
      | Addr a, IntVal i -> (match mem''.(a) with
          | Some (Block (b, n)) -> if i < 0 || i > n then raise ArrayIndexOutOfBoundsEx;
            (Addr (b+i), mem'', outFlow'')
          | _ -> failwith "Not in memory")
      | _ -> failwith "Nth: Should not happen")

(** Evaluate a statement *)
and eval_stat (env, mem, outFlow) = function
  | Echo e -> (match (eval_expr (env, mem, outFlow) e) with (IntVal i, mem', outFlow') -> Empty, mem', (i::outFlow')
                                                 | _ -> failwith "Echo: Should not happen")
  | Set (x, e) -> let (v, mem', outFlow') = eval_expr (env, mem, outFlow) e in
    let (a, mem'', outFlow'') = eval_lval (env, mem', outFlow') x in (match a with
      | Addr a -> mem''.(a) <- Some v; (Empty, mem'', outFlow'')
      | _ -> failwith "Set: Should not happen")
  | Ifs (c, t, e) -> (match (eval_expr (env, mem, outFlow) c) with
      | (IntVal 1, mem', outFlow') -> let (v, mem'', outFlow'') = eval_block (env, mem', outFlow') t in
        (v, (restrict_mem mem'' env), outFlow'')
      | (IntVal 0, mem', outFlow') -> let (v, mem'', outFlow'') = eval_block (env, mem', outFlow') e in
        (v, (restrict_mem mem'' env), outFlow'')
      | _ -> failwith "Ifs: Should not happen")
  | While (c, b) -> (match (eval_expr (env, mem, outFlow) c) with
      | (IntVal 0, mem', outFlow') -> (Empty, mem', outFlow')
      | (IntVal 1, mem', outFlow') -> let (v, mem'', outFlow'') = eval_block (env, mem', outFlow') b in
        (match v with
         | Empty -> eval_stat (env, (restrict_mem mem'' env), outFlow'') (While (c, b))
         | Value _ -> (v, (restrict_mem mem'' env), outFlow''))
      | _ -> failwith "While: Should not happen")
  | Call (x, exprs) -> let p = lookup_env env x in
    let (args, mem', outFlow') = (List.fold_left
                          (fun (a, m, outF) e ->
                            let (v, m', outF') = eval_expr (env, m, outF) e in v::a, m', outF')
                          ([], mem, outFlow) exprs) in
    (match p with
     | ProcClosure (b, r) -> let (v, mem'', outFlow'') = eval_block ((r (List.rev args)), mem', outFlow') b in
       (v, (restrict_mem mem'' env), outFlow'')
     | RecProcClosure rp -> let (b, r) = rp p in
       let (v, mem'', outFlow'') = eval_block ((r (List.rev args)), mem', outFlow') b in
       (v, (restrict_mem mem'' env), outFlow'')
     | _ -> failwith "Call: Should not happen")

(** Evaluate a return command *)
and eval_ret (env, mem, outFlow) (Return e) =
  eval_expr (env, mem, outFlow) e

(** Evaluate a single command (either declaration or statement) *)
and eval_cmds (env, mem, outFlow) = function
  | (Dec d)::cs -> eval_cmds (eval_dec (env, mem, outFlow) d) cs
  | (Stat s)::cs -> let (v, mem', outFlow') = eval_stat (env, mem, outFlow) s in
    (match v with
     | Empty -> eval_cmds (env, mem', outFlow') cs
     | Value _ -> (v, mem', outFlow'))
  | [Ret r] -> let (v, mem', outFlow') = eval_ret (env, mem, outFlow) r in (Value v, mem', outFlow')
  | [] -> (Empty, mem, outFlow)
  | _ -> failwith "cmds: Should not happen"

(** Evaluate a block *)
and eval_block (env, mem, outFlow) b = eval_cmds (env, mem, outFlow) b
  (* let (v, _, mem, outFlow) =
   *   List.fold_left
   *     (fun (_, env, mem, outFlow) cmd -> eval_cmd (env, mem, outFlow) cmd) (Empty, env, mem, outFlow) p in
   * (v, mem, outFlow) *)

(** Evaluate the whole program *)
let eval_prog p =
  let (_, _, outFlow) = eval_block ([], [|None|], []) p in
  List.rev outFlow

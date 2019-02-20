open Ast

type value = IntVal of int | Closure of closure | RecClosure of recclosure
           | Addr of int | ProcClosure of procclosure | RecProcClosure of recprocclosure
           | Block of int * int
and closure = (expr * (value list -> env))
and recclosure = value -> closure
and procclosure = (cmd list * (value list -> env))
and recprocclosure = value -> procclosure
and env = (string * value) list

type 'a memspace = None | Any | Some of 'a
type mem = value memspace array

type outFlow = int list

exception ArrayIndexOutOfBoundsEx

let int_of_ival = function IntVal i -> i | _ -> failwith "Should not happen"

let rec lookup_env env s = match env with
  | (s', v)::_ when s = s' -> v
  | _::q -> lookup_env q s
  | [] -> failwith "Should not happen"

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
      | Block (_, n) -> IntVal n, mem | _ -> failwith "Should not happen")
  | Nth -> (match ((List.hd vals), (List.hd (List.tl vals))) with
      | (Block (a, n), IntVal i) -> if i >= n || i < 0 then raise ArrayIndexOutOfBoundsEx;
        (match mem.(a+i) with
         | Some v -> v, mem
         | Any -> IntVal 0, mem
         | None -> failwith "Not in memory")
      | _ -> failwith "Should not happen")
  | Alloc -> (match (List.hd vals) with
      | IntVal n -> let (a, mem') = allocn mem n in (Block (a, n), mem')
      | _ -> failwith "Should not happen")

(** Evaluate an expression and return a value *)
let rec eval_expr (env, mem) e = match e with
  | Bool b -> if b then IntVal 1, mem else IntVal 0, mem
  | IntConst i -> IntVal i, mem
  | Sym s -> (match (lookup_env env s) with
      | Addr a -> (match (mem.(a)) with
          | Some v -> v, mem
          | Any -> IntVal 0, mem (* On décide qu'en cas de variable non initialisé, la valeur par défaut est 0 *)
          | _ -> failwith (Printf.sprintf "%s is not in memory (addr %d is empty)" s a))
      | v -> v, mem)
  | If (c, t, e) -> (match (eval_expr (env, mem) c) with
      | IntVal 1, mem' -> (eval_expr (env, mem') t)
      | IntVal 0, mem' -> (eval_expr (env, mem') e)
      | _ -> failwith "Should not happen")
  | Op (op, exprs) ->
    let (args, mem) = (List.fold_left (fun (a, m) e -> let (v, m') = eval_expr (env, m) e in v::a, m') ([], mem) exprs) in
    eval_op op (List.rev args) mem
  | App (f, exprs) -> let (c, mem') = eval_expr (env, mem) f in
    let (args, mem'') = (List.fold_left (fun (a, m) e -> let (v, m') = eval_expr (env, m) e in v::a, m') ([], mem') exprs) in
    (match c with
     | Closure (e', r) -> eval_expr ((r (List.rev args)), mem'') e'
     | RecClosure rf -> let (e', r) = (rf c) in eval_expr ((r (List.rev args)), mem'') e'
     | _ -> failwith "Should not happen")
  | Abs (a, e) -> Closure (e, (fun args -> (List.combine (fst (List.split a)) args)@env)), mem

(** Evaluate a declaration *)
and eval_dec (env, mem) = function
  | ConstDec (x, _, e) -> let (v, mem') = eval_expr (env, mem) e in ((x, v)::env, mem')
  | FunDec (x, _, a, e) ->
    let c = Closure (e, fun args -> (List.combine (fst (List.split a)) args)@env)
    in ((x, c)::env, mem)
  | RecFunDec (x, _, a, e) ->
    let rc = RecClosure (fun f ->
        (e, fun args -> (x, f)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rc)::env, mem)
  | VarDec (x, _) -> let (a, mem') = allocn mem 1 in ((x, Addr a)::env, mem')
  | ProcDec (x, a, b) -> let pc = ProcClosure (b, fun args -> (List.combine (fst (List.split a)) args)@env) in
    ((x, pc)::env, mem)
  | RecProcDec (x, a, b) -> let rpc = RecProcClosure (fun p ->
      (b, fun args -> (x, p)::(List.combine (fst (List.split a)) args)@env)) in
    ((x, rpc)::env, mem)

(** Evaluate an left value *)
and eval_lval (env, mem) = function
  | SymLval s -> (match (lookup_env env s) with
      | Addr a -> ((Addr a), mem)
      | Block (b, n) -> (Block (b,n), mem)
      | _ -> failwith "Should not happen")
  | Nth (lval, ie) -> let (b, mem') = eval_lval (env, mem) lval in
    let (iv, mem'') = eval_expr (env, mem') ie in (match b, iv with
      | Block (b, n), IntVal i -> if i < 0 || i > n then raise ArrayIndexOutOfBoundsEx;
        (Addr (b+i), mem'')
      | Addr a, IntVal i -> (match mem''.(a) with
          | Some (Block (b, n)) -> if i < 0 || i > n then raise ArrayIndexOutOfBoundsEx;
            (Addr (b+i), mem'')
          | _ -> failwith "Not in memory")
      | _ -> failwith "Should not happen")

(** Evaluate a statement *)
and eval_stat (env, mem, outFlow) = function
  | Echo e -> (match (eval_expr (env, mem) e) with (IntVal i, mem') -> mem', (i::outFlow)
                                                 | _ -> failwith "Should not happen")
  | Set (x, e) -> let (v, mem') = eval_expr (env, mem) e in
    let (a, mem'') = eval_lval (env, mem') x in (match a with
      | Addr a -> mem''.(a) <- Some v; (mem'', outFlow)
      | _ -> failwith "Should not happen")
  | Ifs (c, t, e) -> (match (eval_expr (env, mem) c) with
      | (IntVal 1, mem') -> let (mem'', outFlow) = eval_block (env, mem', outFlow) t in
        ((restrict_mem mem'' env), outFlow)
      | (IntVal 0, mem') -> let (mem'', outFlow) = eval_block (env, mem', outFlow) e in
        ((restrict_mem mem'' env), outFlow)
      | _ -> failwith "Should not happen")
  | While (c, b) -> (match (eval_expr (env, mem) c) with
      | (IntVal 0, mem') -> (mem', outFlow)
      | (IntVal 1, mem') -> let (mem'', outFlow') = eval_block (env, mem', outFlow) b in
        eval_stat (env, (restrict_mem mem'' env), outFlow') (While (c, b))
      | _ -> failwith "Should not happen")
  | Call (x, exprs) -> let p = lookup_env env x in
    let (args, mem') = (List.fold_left (fun (a, m) e -> let (v, m') = eval_expr (env, m) e in v::a, m') ([], mem) exprs) in
    (match p with
     | ProcClosure (b, r) -> let (mem'', outFlow') = eval_block ((r (List.rev args)), mem', outFlow) b in
       ((restrict_mem mem'' env), outFlow')
     | RecProcClosure rp -> let (b, r) = rp p in
       let (mem'', outFlow') = eval_block ((r (List.rev args)), mem', outFlow) b in
       ((restrict_mem mem'' env), outFlow')
     | _ -> failwith "Should not happen")

(** Evaluate a single command (either declaration or statement) *)
and eval_cmd (env, mem, outFlow) = function
  | Dec d -> let (env, mem) = eval_dec (env, mem) d in (env, mem, outFlow)
  | Stat s -> let (mem, outFlow) = eval_stat (env, mem, outFlow) s in (env, mem, outFlow)

(** Evaluate a block *)
and eval_block startState p =
  let (_, mem, outFlow) = List.fold_left eval_cmd startState p in
  (mem, outFlow)

(** Evaluate the whole program *)
let eval_prog p =
  let (_, outFlow) = eval_block ([], [|None|], []) p in
  List.rev outFlow

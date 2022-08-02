open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
| Value(v) -> v
| ID(id) -> lookup env id
| Not(expr) -> (match eval_expr env expr with
  | Bool(b) -> Bool(not b)
  | _ -> raise(TypeError "not bool/id!"))
| Binop(op, e1, e2) -> (match op with
  | Add -> 
    let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Int (i1 + i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | Sub -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Int (i1 - i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | Mult -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Int (i1 * i2)
    | _ -> raise(TypeError "Not Two ints!")) 
  | Div -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> if i2 = 0 then raise(DivByZeroError) else Int(i1 / i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | Greater -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 > i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | Less -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 < i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | GreaterEqual -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 >= i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | LessEqual -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 <= i2)
    | _ -> raise(TypeError "Not Two ints!"))
  | Concat -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (String s1, String s2) -> String(s1^s2)
    | _ -> raise(TypeError "Not Two Strings!"))
  | Equal -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Bool(i1 = i2)
    | (String s1, String s2) -> Bool(s1 = s2)
    | (Bool b1, Bool b2) -> Bool(b1 = b2)
    | _ -> raise(TypeError "Arguments aren't the same type!"))
  | NotEqual -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Int i1, Int i2) -> Bool(i1 <> i2)
    | (String s1, String s2) -> Bool(s1 <> s2)
    | (Bool b1, Bool b2) -> Bool(b1 <> b2)
    | _ -> raise(TypeError "Arguments aren't the same type!"))
  | Or -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Bool b1, Bool b2) -> Bool(b1 || b2)
    | _ -> raise(TypeError "Didn't find two bools!"))
  | And -> let val1 = eval_expr env e1 in
    let val2 = eval_expr env e2 in
    (match (val1, val2) with
    | (Bool b1, Bool b2) -> Bool(b1 && b2)
    | _ -> raise(TypeError "Didn't find two bools!")))
| If(guard, e1, e2) -> let check_guard = eval_expr env guard in 
  (match check_guard with
  | Bool b -> if b then eval_expr env e1 else eval_expr env e2
  | _ -> raise(TypeError "Guard isn't a bool!"))

| Let(id, is_rec, init, body) -> 
    if is_rec = false then let eval_init = eval_expr env init in
    eval_expr (extend env id eval_init) body
    else let temp_env = extend_tmp env id in 
    let mapped_value = eval_expr temp_env init in
    update temp_env id mapped_value;
    eval_expr temp_env body

| Fun(param, body) -> Closure(env, param, body)

| FunctionCall(subexpr1, subexpr2) ->
  let evaluated_subexpr2 = eval_expr env subexpr2 in
  match eval_expr env subexpr1 with
  | Closure(new_env,x,e) -> 
    let extended_env = extend new_env x evaluated_subexpr2 in
    eval_expr extended_env e
  | _ -> raise(TypeError "not a closure")

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
| NoOp -> (env, None)
| Expr expr -> (match eval_expr env expr with 
  | Int int -> (env, Some(Int(int)))
  | Bool bool -> (env, Some(Bool(bool)))
  | String string -> (env, Some(String(string)))
  | Closure (environment, var, expr) -> (environment, Some(Closure(environment, var, expr))))
| Def (var, expr) -> (
  let temp_env = extend_tmp env var in 
  let new_val = eval_expr temp_env expr in
  let () = update temp_env var new_val in 
  match new_val with 
  | Int int -> (temp_env, Some(Int(int)))
  | Bool bool -> (temp_env, Some(Bool(bool)))
  | String string -> (temp_env, Some(String(string)))
  | Closure (_, var, expr) -> (temp_env, Some(Closure(temp_env, var, expr))))

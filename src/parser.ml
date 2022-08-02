open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = parse_general_expr toks

and parse_general_expr tokens = match lookahead tokens with
  | None -> parse_or_expr tokens
  | Some tok -> match tok with
    | Tok_Let -> parse_let_expr tokens
    | Tok_If -> parse_if_expr tokens
    | Tok_Fun -> parse_fun_expr tokens
    | _ -> parse_or_expr tokens

and parse_let_expr tokens = let removed_let_token = match_token tokens Tok_Let in
  match lookahead removed_let_token with
  | None -> raise(InvalidInputException("Nothing after the let!"))
  | Some tok -> (match tok with
    | Tok_Rec -> (let removed_rec_token = match_token removed_let_token Tok_Rec in
      match lookahead removed_rec_token with
      | None -> raise(InvalidInputException("Incomplete let parse!"))
      | Some tok2 ->
        match tok2 with
        | Tok_ID(id) -> let removed_id = match_token removed_rec_token (Tok_ID(id)) in 
          let removed_equal = match_token removed_id Tok_Equal in
          let (rem_toks1, expr1) = parse_general_expr removed_equal in
          let removed_in = match_token rem_toks1 Tok_In in
          let (rem_toks2, expr2) = parse_general_expr removed_in in
          (rem_toks2, Let(id, true, expr1, expr2))
        | _ -> raise(InvalidInputException("No ID found")))
         
    | Tok_ID(id) -> let removed_id = match_token removed_let_token (Tok_ID(id)) in 
          let removed_equal = match_token removed_id Tok_Equal in
          let (rem_toks1, expr1) = parse_general_expr removed_equal in
          let removed_in = match_token rem_toks1 Tok_In in
          let (rem_toks2, expr2) = parse_general_expr removed_in in
          (rem_toks2, Let(id, false, expr1, expr2))

    | _ -> raise(InvalidInputException("No rec or id found")))

and parse_if_expr tokens = let removed_if = match_token tokens Tok_If in
  let (rem_toks1, expr1) = parse_general_expr removed_if in
  let removed_then = match_token rem_toks1 Tok_Then in
  let (rem_toks2, expr2) = parse_general_expr removed_then in
  let removed_else = match_token rem_toks2 Tok_Else in
  let (rem_toks3, expr3) = parse_general_expr removed_else in (rem_toks3, If(expr1, expr2, expr3))

and parse_fun_expr tokens = let removed_fun = match_token tokens Tok_Fun in
match lookahead removed_fun with
| None -> raise(InvalidInputException("Nothing after the fun!"))
| Some tok -> match tok with
  | Tok_ID(id) -> let removed_id = match_token removed_fun (Tok_ID(id)) in
    let removed_arrow = match_token removed_id Tok_Arrow in
    let (rem_toks, expr) = parse_general_expr removed_arrow in (rem_toks, Fun(id, expr))
  | _ -> raise(InvalidInputException("No ID Found"))

and parse_or_expr tokens = let (rem_toks, expr) = parse_and_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some tok -> match tok with
    | Tok_Or -> let remove_or_token = match_token rem_toks Tok_Or in
      let (rem_toks2, expr2) = parse_or_expr remove_or_token in (rem_toks2, Binop(Or, expr, expr2))
    | _ -> (rem_toks, expr)

and parse_and_expr tokens = let (rem_toks, expr) = parse_equality_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some tok -> match tok with
    | Tok_And -> let remove_and_token = match_token rem_toks Tok_And in
      let (rem_toks2, expr2) = parse_and_expr remove_and_token in (rem_toks2, Binop(And, expr, expr2))
    | _ -> (rem_toks, expr)

and parse_equality_expr tokens = let (rem_toks, expr) = parse_relational_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some equality_operator -> match equality_operator with
    | Tok_NotEqual -> let remove_notequal_token = match_token rem_toks Tok_NotEqual in
      let (rem_toks2, expr2) = parse_equality_expr remove_notequal_token in (rem_toks2, Binop(NotEqual, expr, expr2))
    | Tok_Equal -> let remove_equal_token = match_token rem_toks Tok_Equal in
      let (rem_toks2, expr2) = parse_equality_expr remove_equal_token in (rem_toks2, Binop(Equal, expr, expr2)) 
    | _ -> (rem_toks, expr)

and parse_relational_expr tokens = let (rem_toks, expr) = parse_additive_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some relational_operator -> match relational_operator with
    | Tok_Greater -> let remove_greater_token = match_token rem_toks Tok_Greater in
      let (rem_toks2, expr2) = parse_relational_expr remove_greater_token in (rem_toks2, Binop(Greater, expr, expr2))
    | Tok_GreaterEqual -> let remove_greaterequal_token = match_token rem_toks Tok_GreaterEqual in
      let (rem_toks2, expr2) = parse_relational_expr remove_greaterequal_token in (rem_toks2, Binop(GreaterEqual, expr, expr2)) 
    | Tok_Less -> let remove_less_token = match_token rem_toks Tok_Less in
      let (rem_toks2, expr2) = parse_relational_expr remove_less_token in (rem_toks2, Binop(Less, expr, expr2))
    | Tok_LessEqual -> let remove_lessequal_token = match_token rem_toks Tok_LessEqual in
      let (rem_toks2, expr2) = parse_relational_expr remove_lessequal_token in (rem_toks2, Binop(LessEqual, expr, expr2))
    | _ -> (rem_toks, expr)

and parse_additive_expr tokens = let (rem_toks, expr) = parse_multiplicative_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some additive_operator -> match additive_operator with
    | Tok_Add -> let remove_add_token = match_token rem_toks Tok_Add in
      let (rem_toks2, expr2) = parse_additive_expr remove_add_token in (rem_toks2, Binop(Add, expr, expr2))
    | Tok_Sub -> let remove_sub_token = match_token rem_toks Tok_Sub in
      let (rem_toks2, expr2) = parse_additive_expr remove_sub_token in (rem_toks2, Binop(Sub, expr, expr2))
    | _ -> (rem_toks, expr)

and parse_multiplicative_expr tokens = let (rem_toks, expr) = parse_concat_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some multiplicative_operator -> match multiplicative_operator with
    | Tok_Mult -> let remove_mult_token = match_token rem_toks Tok_Mult in
      let (rem_toks2, expr2) = parse_multiplicative_expr remove_mult_token in (rem_toks2, Binop(Mult, expr, expr2))
    | Tok_Div -> let remove_div_token = match_token rem_toks Tok_Div in
      let (rem_toks2, expr2) = parse_multiplicative_expr remove_div_token in (rem_toks2, Binop(Div, expr, expr2))
    | _ -> (rem_toks, expr)

and parse_concat_expr tokens = let (rem_toks, expr) = parse_unary_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some tok -> match tok with
    | Tok_Concat -> let remove_concat_token = match_token rem_toks Tok_Concat in
      let (rem_toks2, expr2) = parse_concat_expr remove_concat_token in (rem_toks2, Binop(Concat, expr, expr2))
    | _ -> (rem_toks, expr)

and parse_unary_expr tokens = match lookahead tokens with
  | None -> parse_functioncall_expr tokens
  | Some tok -> match tok with
    | Tok_Not -> let remove_not_token = match_token tokens Tok_Not in
      let (rem_toks2, expr2) = parse_unary_expr remove_not_token in (rem_toks2, Not(expr2))
    | _ -> parse_functioncall_expr tokens

and parse_functioncall_expr tokens = let (rem_toks, expr) = parse_primary_expr tokens in
  match lookahead rem_toks with
  | None -> (rem_toks, expr)
  | Some tok -> match tok with
    | Tok_Int(num) -> let (rem_toks2, expr2) = parse_primary_expr rem_toks in (rem_toks2, FunctionCall(expr, expr2))
    | Tok_Bool(b) -> let (rem_toks2, expr2) = parse_primary_expr rem_toks in (rem_toks2, FunctionCall(expr, expr2))
    | Tok_String(str) -> let (rem_toks2, expr2) = parse_primary_expr rem_toks in (rem_toks2, FunctionCall(expr, expr2))
    | Tok_ID(id) -> let (rem_toks2, expr2) = parse_primary_expr rem_toks in (rem_toks2, FunctionCall(expr, expr2))
    | Tok_LParen -> let (rem_toks2, expr2) = parse_primary_expr rem_toks in (rem_toks2, FunctionCall(expr, expr2))
    | _ -> (rem_toks, expr)

and parse_primary_expr tokens = match lookahead tokens with
| None -> raise(InvalidInputException("No Lookahead Token!"))
| Some tok -> match tok with
  
  | Tok_Int(num) -> let tokens = match_token tokens (Tok_Int(num)) in (tokens, Value(Int(num)))
  | Tok_String(str) -> let tokens = match_token tokens (Tok_String(str)) in (tokens, Value(String(str)))
  | Tok_Bool(b) -> let tokens = match_token tokens (Tok_Bool(b)) in (tokens, Value(Bool(b)))
  | Tok_ID(id) -> let tokens = match_token tokens (Tok_ID(id)) in (tokens, ID(id))
  | Tok_LParen -> let tokens = match_token tokens Tok_LParen in 
      (match parse_general_expr tokens with
      | (toks, exp) -> let tokens = match_token toks Tok_RParen in (tokens, exp))
  
  | _ -> raise(InvalidInputException("Invalid Primary Token"))



(* Part 3: Parsing mutop *)

let rec parse_mutop toks = parse_mutop_expr toks

and parse_mutop_expr tokens = match lookahead tokens with 
| None -> raise(InvalidInputException("Nothing Left!"))
| Some tok -> match tok with
  | Tok_Def -> parse_def_expr tokens
  | Tok_DoubleSemi -> ([], NoOp)
  | _ -> parse_exprmutop_expr tokens


and parse_def_expr tokens = let remove_def = match_token tokens Tok_Def in
  match lookahead remove_def with
  | None -> failwith "No ID!"
  | Some tok -> match tok with
    | Tok_ID(id) -> let remove_id = match_token remove_def (Tok_ID(id)) in
      let remove_equal = match_token remove_id Tok_Equal in
      let (rem_toks, expr) = parse_general_expr remove_equal in 
      let removed_doublesemi = match_token rem_toks Tok_DoubleSemi in (removed_doublesemi, Def(id, expr))
    | _ -> failwith "No ID!"

and parse_exprmutop_expr tokens = let (rem_toks, expr) = parse_general_expr tokens in
let removed_doublesemi = match_token rem_toks Tok_DoubleSemi in (removed_doublesemi, Expr(expr))
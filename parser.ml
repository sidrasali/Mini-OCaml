open Lexer

type var = string
type con = Bcon of bool | Icon of int | Scon of string 
type op  = Add | Sub | Mul | Leq 
type exp = 
  | Var of var (* Variable *)
  | Con of con (* Constant *)
  | Oapp of op * exp * exp (* Operator application *)
  | If of exp * exp * exp (* If expression *)
  | Let of var * exp * exp (* Let binding *)

(* Helper function to verify a token exists *)
let verify expected tokens = 
  match tokens with
  | t' :: rest when t' = expected -> rest
  | _ -> failwith "Token mismatch or missing"

(* Parsing functions to generate an AST from tokens *)
let rec parse_exp tokens = 
  match tokens with
  | IF :: rest ->
      let cond, rest = parse_exp rest in
      let then_branch, rest = parse_exp (verify THEN rest) in
      let else_branch, rest = parse_exp (verify ELSE rest) in
      If (cond, then_branch, else_branch), rest
  | LET :: VAR name :: EQ :: rest ->
    let value, rest = parse_exp rest in
    (match rest with
      | IN :: rest -> 
          let body, rest = parse_exp rest in
          Let (name, value, body), rest
      | _ -> Let (name, value, Con (Scon "")), rest)  (* Handle standalone let *)    
  | _ -> parse_op tokens
and parse_op tokens =
  let lhs, rest = parse_term tokens in
  parse_op_tail lhs rest
and parse_op_tail lhs tokens =
  match tokens with
  | LEQ :: rest ->
      let rhs, rest = parse_term rest in
      parse_op_tail (Oapp (Leq, lhs, rhs)) rest
  | ADD :: rest ->
      let rhs, rest = parse_term rest in
      parse_op_tail (Oapp (Add, lhs, rhs)) rest
  | SUB :: rest ->
      let rhs, rest = parse_term rest in
      parse_op_tail (Oapp (Sub, lhs, rhs)) rest
  | _ -> lhs, tokens 
and parse_term tokens =
  let lhs, rest = parse_factor tokens in
  parse_term_tail lhs rest
and parse_term_tail lhs tokens =
  match tokens with
  | MUL :: rest ->
      let rhs, rest = parse_factor rest in
      parse_term_tail (Oapp (Mul, lhs, rhs)) rest
  | _ -> lhs, tokens
and parse_factor tokens =
  match tokens with
  | CON (BCON b) :: rest -> Con (Bcon b), rest
  | CON (ICON n) :: rest -> Con (Icon n), rest
  | CON (SCON s) :: rest -> Con (Scon s), rest 
  | VAR name :: rest -> Var name, rest
  | LP :: rest ->
      let expr, rest = parse_exp rest in
      expr, verify RP rest
  | _ -> failwith "Unexpected token in factor"

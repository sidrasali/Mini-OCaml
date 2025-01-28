(* Language and syntax definitions *)
type const = BCON of bool | ICON of int | SCON of string 
type token = ADD | SUB | MUL | LP | RP | EQ | LEQ 
           | IF | THEN | ELSE | LET | IN 
           | CON of const | VAR of string

(* Helper functions for lexical analysis *)
let num c = Char.code c - Char.code '0'
let is_digit c = '0' <= c && c <= '9'
let is_lc_letter c = 'a' <= c && c <= 'z'
let is_uc_letter c = 'A' <= c && c <= 'Z'
let is_whitespace c = match c with
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

(* Lexer function to tokenize a given input string *)
let lex input =
  let len = String.length input in
  let get index = String.get input index in
  let rec lex index tokens =
    if index >= len then List.rev tokens
    else match get index with
      | '+' -> lex (index + 1) (ADD :: tokens)
      | '*' -> lex (index + 1) (MUL :: tokens)
      | '=' -> lex (index + 1) (EQ :: tokens)
      | '(' -> lex (index + 1) (LP :: tokens)
      | ')' -> lex (index + 1) (RP :: tokens)
      | '"' -> lex_string (index + 1) "" tokens
      | '<' when index + 1 < len && get (index + 1) = '=' -> lex (index + 2) (LEQ :: tokens)
      | c when is_whitespace c -> lex (index + 1) tokens
      | c when is_digit c -> lex_num (index + 1) (num c) tokens
      | c when is_lc_letter c -> lex_identifier (index + 1) (String.make 1 c) tokens
      | _ -> failwith "Illegal character"
  and lex_string index acc tokens =
    if index < len && get index <> '"' then
      lex_string (index + 1) (acc ^ String.make 1 (get index)) tokens
    else lex (index + 1) (CON (SCON acc) :: tokens)  (* String literal *) 
  and lex_num index acc tokens =
    if index < len && is_digit (get index) then lex_num (index + 1) (10 * acc + num (get index)) tokens
    else lex index (CON (ICON acc) :: tokens)
  and lex_identifier index acc tokens =
    if index < len && (is_lc_letter (get index) || is_digit (get index)) then
      lex_identifier (index + 1) (acc ^ String.make 1 (get index)) tokens
    else lex index (match acc with
        | "if" -> IF :: tokens
        | "then" -> THEN :: tokens
        | "else" -> ELSE :: tokens
        | "let" -> LET :: tokens
        | "in" -> IN :: tokens
        | "true" -> CON (BCON true) :: tokens
        | "false" -> CON (BCON false) :: tokens
        | _ -> VAR acc :: tokens)
  in
  lex 0 []
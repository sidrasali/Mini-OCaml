open Lexer
open Parser
open Evaluator

(* Print the evaluation result *)
let interpret input =
  try
    let tokens = lex input in
    let ast, _ = parse_exp tokens in
    let result = eval empty_env ast in
    let result_str = match result with
      | Ival n -> string_of_int n
      | Bval b -> string_of_bool b
      | Sval s -> s
    in
    Printf.printf "The expression \"%s\" evaluates to \"%s\"\n" input result_str
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | _ -> Printf.printf "Unknown error\n"

;;

interpret "let message = \"ocaml is fun\" in message"
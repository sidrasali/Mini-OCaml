open Parser 

(* Environment implementation for variable binding *)
type ('a, 'b) env = ('a * 'b) list
let empty_env = []
let update_env env key value = (key, value) :: env
let rec lookup_env env key = 
  match env with
  | (k, v) :: _ when k = key -> Some v
  | _ :: rest -> lookup_env rest key
  | [] -> None 

(* Values for evaluation *)
type value = Bval of bool | Ival of int | Sval of string

(* Evaluation functions *)
let eval_op op v1 v2 =
  match op, v1, v2 with
  | Add, Ival n1, Ival n2 -> Ival (n1 + n2)
  | Sub, Ival n1, Ival n2 -> Ival (n1 - n2)
  | Mul, Ival n1, Ival n2 -> Ival (n1 * n2)
  | Leq, Ival n1, Ival n2 -> Bval (n1 <= n2)
  | _ -> failwith "Invalid operation"

let rec eval env expr =
  match expr with
  | Var name -> (match lookup_env env name with
      | Some value -> value
      | None -> failwith "Unknown variable")
  | Con (Bcon b) -> Bval b
  | Con (Icon n) -> Ival n
  | Con (Scon s) -> Sval s 
  | Oapp (op, lhs, rhs) -> eval_op op (eval env lhs) (eval env rhs)
  | If (cond, then_branch, else_branch) ->
      (match eval env cond with
       | Bval true -> eval env then_branch
       | Bval false -> eval env else_branch
       | _ -> failwith "Condition must be Bool")
  | Let (name, value, body) ->
      let value_eval = eval env value in
      eval (update_env env name value_eval) body

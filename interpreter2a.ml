(* 式の型 *)
type exp =
| IntLit of int
| Plus of exp * exp
| Times of exp * exp
| Minus of exp * exp
| Div of exp * exp
| BoolLit of bool
| If of exp * exp * exp
| Eq of exp * exp
| Greater of exp * exp

(* 値の型 *)
type value =
| IntVal of int
| BoolVal of bool

(* eval2a: exp -> value *)
let rec eval2a e =
  match e with
  | IntLit n -> IntVal n
  | Plus (e1, e2) -> binop 1 e1 e2
  | Times (e1, e2) -> binop 2 e1 e2
  | _ -> failwith "unknown expression e"
and binop flag e1 e2 =
  match (eval2a e1, eval2a e2) with
  | (IntVal n1, IntVal n2) ->
    if flag = 1 then IntVal (n1 + n2)
    else IntVal (n1 * n2)
  | _ -> failwith "integer values expected"
(* 処理対象となる式の型の定義 *)
type exp =
| IntLit of int (* 整数リテラル *)
| Plus of exp * exp (* 足し算 *)
| Times of exp * exp (* 掛け算 *)
| Minus of exp * exp (* 引き算 *)
| Div of exp * exp (* 割り算 *)


(* eval1: exp -> int *)
let rec eval1 e =
  match e with
  | IntLit(n) -> n
  | Plus(e1, e2) -> (eval1 e1) + (eval1 e2)
  | Times(e1, e2) -> (eval1 e1) * (eval1 e2)
  | Minus(e1, e2) -> (eval1 e1) - (eval1 e2)
  | Div(e1, e2) ->
    match e2 with
    | IntLit 0 -> failwith "unknown expression"
    | IntLit _ -> (eval1 e1) / (eval1 e2) 
  | _ -> failwith "unknown expression"


let is_literal e =
  match e with
  | IntLit e -> true
  | _ -> false


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

(* eval2e: exp -> value *)
let rec eval2e e =
  let binop f e1 e2 =
    match (eval2e e1, eval2e e2) with
    | (IntVal n1, IntVal n2) -> f n1 n2
    | _ -> failwith "integer values expected"
  in
  let arith op e1 e2 =
    binop (fun n1 n2 -> IntVal (op n1 n2)) e1 e2
  in
  let compare op e1 e2 =
    binop (fun n1 n2 -> BoolVal (op n1 n2)) e1 e2
  in
  match e with
  | IntLit n -> IntVal n
  | Plus(e1, e2) ->  arith (+) e1 e2
  | Times(e1, e2) -> arith (-) e1 e2
  | Minus(e1, e2) -> arith ( * ) e1 e2
  | Div(e1, e2) -> 
    binop 
    (fun n1 n2 ->
      if n2 = 0 then failwith "zero division error"
      else IntVal (n1 / n2))
    e1 e2

  | Eq(e1, e2) -> compare (=) e1 e2

  | BoolLit b -> BoolVal b

  | If(e1, e2, e3) ->
    begin
      match (eval2e e1) with
      | BoolVal true -> eval2e e2
      | BoolVal false -> eval2e e3
      | _ -> failwith "wrong value"
    end

  | Greater(e1, e2) -> compare (>) e1 e2 

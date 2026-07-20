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

(* eval3: exp -> value *)
let rec eval3 e =
  let binop f e1 e2 =
    match (eval3 e1, eval3 e2) with
    | (IntVal n1, IntVal n2) -> IntVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  let binop_b f e1 e2 =
    match (eval3 e1, eval3 e2) with
    | (IntVal n1, IntVal n2) -> BoolVal (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit n -> IntVal n
  | Plus(e1, e2) -> binop (+) e1 e2
  | Times(e1, e2) -> binop ( * ) e1 e2
  | Minus(e1, e2) -> binop (-) e1 e2
  | Div(e1, e2) ->
    begin
      match (eval3 e1, eval3 e2) with
      | (IntVal n1, IntVal n2) ->
        begin
          match n2 with
          | 0 -> failwith "zero division error"
          | _ -> IntVal (n1 / n2)
        end
      | _ -> failwith "integer values expected"
      end
    
  | Eq(e1, e2) ->binop_b (=) e1 e2

  | BoolLit b -> BoolVal b

  | If(e1, e2, e3) ->
    begin
      match (eval3 e1) with
      | BoolVal true -> eval3 e2
      | BoolVal false -> eval3 e3
      | _ -> failwith "wrong value"
    end

  | Greater(e1, e2) ->binop_b (>) e1 e2
  
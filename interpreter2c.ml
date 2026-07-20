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

(* eval2c: exp -> value *)
let rec eval2c e =
  let binop f make_value e1 e2 =
    match (eval2c e1, eval2c e2) with
    | (IntVal n1, IntVal n2) -> make_value (f n1 n2)
    | _ -> failwith "integer values expected"
  in
  match e with
  | IntLit n -> IntVal n
  | Plus(e1, e2) -> binop (+) (fun n -> IntVal n) e1 e2
  | Times(e1, e2) -> binop ( * ) (fun n -> IntVal n) e1 e2
  | Minus(e1, e2) -> binop (-)  (fun n -> IntVal n) e1 e2
  | Div(e1, e2) ->
    begin
      match (eval2c e1, eval2c e2) with
      | (IntVal n1, IntVal n2) ->
        begin
          match n2 with
          | 0 -> failwith "zero division error"
          | _ -> IntVal (n1 / n2)
        end
      | _ -> failwith "integer values expected"
      end
    
  | Eq(e1, e2) ->binop (=) (fun n -> BoolVal n) e1 e2

  | BoolLit b -> BoolVal b

  | If(e1, e2, e3) ->
    begin
      match (eval2c e1) with
      | BoolVal true -> eval2c e2
      | BoolVal false -> eval2c e3
      | _ -> failwith "wrong value"
    end

  | Greater(e1, e2) ->binop (>) (fun n -> BoolVal n) e1 e2

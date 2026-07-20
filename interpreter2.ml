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

(* eval2: exp -> value *)
let rec eval2 e =
  match e with
  | IntLit n -> IntVal n

  | Plus(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
      | _ -> failwith "integer values expected"
    end

  | Times(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
      | _ -> failwith "integer values expected"
    end

  | Minus(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
      | _ -> failwith "integer values expected"
    end

  | Div(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal n1, IntVal n2) ->
        begin
          match n2 with
          | 0 -> failwith "zero division error"
          | _ -> IntVal (n1 / n2)
        end
      | _ -> failwith "integer values expected"
    end

  | Eq(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 = n2)
      | (BoolVal b1, BoolVal b2) -> BoolVal (b1 = b2)
      | _ -> failwith "wrong value"
    end

  | BoolLit b -> BoolVal b

  | If(e1, e2, e3) ->
    begin
      match (eval2 e1) with
      | BoolVal true -> eval2 e2
      | BoolVal false -> eval2 e3
      | _ -> failwith "wrong value"
    end

  | Greater(e1, e2) ->
    begin
      match (eval2 e1, eval2 e2) with
      | (IntVal n1, IntVal n2) -> BoolVal (n1 > n2)
      | _ -> failwith "wrong value"
    end

  | _ -> failwith "unknown expression e"

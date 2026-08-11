(* inputは、まだ読み取っていない文字のリストを表す *)
type input = char list

(* chars_of_string : string -> input *)
let chars_of_string s = 
  List.of_seq (String.to_seq s)








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
| Var of string
| Let of string * exp * exp

(* 値の型 *)
type value =
| IntVal of int
| BoolVal of bool

(* 環境の作成、更新、環境 *)
let emptyenv () = []
let ext env x v = (x, v) :: env
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable:" ^ x)
  | (y, v)::tl -> if x = y then v
                  else lookup x tl

(* eval3: exp -> (string * value) list -> value *)
let rec eval3 e env = (* envを引数に追加 *)
  (* binopの形式は、interpreter2d.mlを使うことにした *)
  let binop f e1 e2 env = (* binopの中でもeval3を呼ぶのでenvを追加 *)
  match (eval3 e1 env, eval3 e2 env) with
  | (IntVal n1, IntVal n2) -> f n1 n2
  | _ -> failwith "integer value expected"
  in
  match e with
  | Var x -> lookup x env
  | IntLit n -> IntVal n
  | BoolLit n -> BoolVal n
  | Plus(e1, e2) -> binop (fun n1 n2 -> IntVal (n1 + n2)) e1 e2 env
  | Times(e1, e2) -> binop (fun n1 n2 -> IntVal (n1 * n2)) e1 e2 env
  | Minus(e1, e2) -> binop (fun n1 n2 -> IntVal (n1 - n2)) e1 e2 env
  | Div(e1, e2) -> binop (fun n1 n2 -> if n2 = 0 then failwith "zero division error"
                                       else IntVal (n1 / n2)) e1 e2 env
  | Eq(e1, e2) -> binop (fun n1 n2 -> BoolVal (n1 = n2)) e1 e2 env
  | Greater(e1, e2) -> binop (fun n1 n2 -> BoolVal (n1 > n2)) e1 e2 env
  | If(e1, e2, e3) ->
    begin
      match (eval3 e1 env) with
      | BoolVal true -> eval3 e2 env
      | BoolVal false -> eval3 e3 env
      | _ -> failwith "wrong value"
    end
  | Let(x, e1, e2) ->
    let env1 = ext env x (eval3 e1 env)
    in eval3 e2 env1

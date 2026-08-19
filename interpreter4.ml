(* inputは、まだ読み取っていない文字のリストを表す *)
type input = char list

(* stringで受け取ったソースコード列をinput型(charのリスト)に変換する *)
(* chars_of_string : string -> input *)
let chars_of_string s = 
  List.of_seq (String.to_seq s)

(* 条件condを満たすなら、次の1文字を読み取る *)
(* satisfy: (char -> bool) -> input -> (char * input) option *)
let satisfy cond input = 
  match input with
  | c :: cs when cond c -> Some (c, cs)
  | _ -> None

(* 指定した文字cと一致するかどうかを判定する *)
(* char_p: char -> input -> (char * input) option *)
let char_p c input =
  satisfy (fun x -> x = c) input

(* スペースを飛ばす *)
(* skip_spaces: input -> input *)
let rec skip_spaces input =
  match input with
  (* ' 'はスペース、'\t'はタブ、'\n'は改行、'\r'はキャリッジリターン(windowsなどの改行コードの一部)を表す *)
  | c :: cs when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
    skip_spaces cs (* 空白だったので、残りのリストcsに対して再帰的に処理を続ける *)
  | _ -> input (* 空白以外の文字が来たら、あるいはリストが空になったら、現在のリストをそのまま帰す *)

(* 数字の列が続く限り、束ねて整数に変換する *)
(* int_p; input -> (int * input) option *)
let int_p input =
  let is_digit c =
    c >= '0' && c <= '9' in
  
  (* 補助関数、数字が続く限りacc(アキュムレータ)に文字を溜め込む *)
  (* read_digits : input -> char list -> char list * input *)
  let rec read_digits chars acc =
    match chars with
    | c :: cs when is_digit c -> read_digits cs (c :: acc)
    | _ -> (List.rev acc, chars)
  in

  match read_digits input [] with
  | ([], _) -> None (*アキュムレータが空 = 最初から数字が1つもなかった場合 *)
  | (digits, rest) ->
    (* 文字のリストを文字列(string)に変換してから、整数(int)に変換する *)
    let str = String.of_seq (List.to_seq digits) in
    Some (int_of_string str, rest)

(* +や*といった指定された文字と、入力の先頭が一致するか確認し、一致したら、その文字列をトークンとして返す *)
(* keyword_p: string -> input -> (string * input) option *)
let keyword_p kw input =

  (* 補助関数、targetの文字のリストと、現在の入力charsが順番に一致するか確認する *)
  (* check_chars : input -> char list -> (string * input) option *)
  let rec check_chars chars target =
    match target with
    | [] -> Some (kw, chars) (* 探したい文字(target)をすべて読み終えたら、一致したとみなして成功 *)
    | t :: ts ->
      (match char_p t chars with
      | Some (_, rest) -> check_chars rest ts (* 1文字一致したので、残りを再帰的に確認する*)
      | None -> None (* 途中で一致しない文字があったら失敗*)) 
    in

  (* 探したい文字列kwを文字のリストに変換してから、check_charsに渡す *)
  check_chars input (List.of_seq (String.to_seq kw))











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

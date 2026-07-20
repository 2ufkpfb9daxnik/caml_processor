(* 例題ファイル1 *)

(* This is a comment.
   ここに書いてあるものはコメントです。
   コメントは2行以上でも構いません。
 *)

(* 整数型の式のいろいろ *)
(* 1つの式の終わりには ;; が必要 *)
1 + 2 * (3 / 2) -1 ;;

(* ;; を書くまでは、2行以上でも1つの式 *)
1 + 2 * 
  (3 / 
   2) -1 ;;

(* 整数型と実数型をまぜてはいけない *)
1 + 2.0 ;;

(* 変数は let で導入できる  *)
let x = 1 in x + 2 ;;

(* let は入れ子(ネスト)でもよい  *)
let x = (let y = 2 in y * 2) in 
    let z = x + 3 in x + z ;;

(* let で導入する変数が同じ場合、内側が有効  *)
let x = 1 in 
   let x = 2 in 
     x ;;

(* ややこしい例  *)
let x = 1 in 
   let x = (let x = x + 1 in x * 2) in 
     x + 2 ;;

(* 実は in 以下を書かなくてもよい  *)
(* その場合、let は最後までずっと有効  *)
x ;;  (* これはエラー *)
let x = 1 ;;
x * 2 ;; (* これは OK *)

(* if then else も使える  *)
let x = 1 in
  if x * 2 = x + x then "ok"
  else "ng" ;;

(* if then else もネストできる  *)
let x = 1 in
  if x * 2 = x + x then 
    if x + 3 > 3 then "ok"
    else "ng"
  else "ng" ;;

(* ただし、then と else で違う型の結果にはできない  *)
let x = 1 in
  if x * 2 > 1  then  3
  else 4.0 ;;

(* また、else 以下がないのは駄目 *)
let x = 1 in
  if x * 2 = x + x then "ok";;

(* 関数のいろいろな定義法 *)
let f x = x + 1 in
  f (f 3) ;;

(* 関数の別の定義方法; 上と全く同じ *)
let f = fun x -> x + 1 in
  f (f 3) ;;

(* 関数だって、ネストできる *)
let f = fun x -> fun y -> x + y in
  (f 3) 4 ;;

(* 関数型言語だから、関数も単なるデータとして、
   他の関数の引数になれる *)
let compose f g x = f (g x) in
let f x = x + 1 in
let g y = y * 3 in
    compose f g 10 ;;

(* 上と同様に、関数をデータとして扱う *)
let double f x = f (f x) in
let f x = x + 1 in
  double (double f) ;;

(* 関数をデータにできるといっても、この例にはちょっと驚くかもしれない *)
let double f x = f (f x) in
let f x = x + 1 in
  (double double) f ;;

(* 再帰関数は、今までの構文では定義できない *)
let f = fun x -> if x = 0 then 1 else x * (f (x - 1)) in 
  f 3 ;;  (* これではエラー *)

(* 再帰関数の定義には、特別なキーワードrec が必要 
   次の関数 f は、階乗を計算する *)
let rec f x = if x = 0 then 1 else x * (f (x - 1)) in 
  f 3 ;;  

(* ところで、真理値(bool)を表すデータも使える *)
true ;;
1 = 2 ;;
let x = 1 in 
  if x > 0 then "big" else "small" ;;

(* = はなぜか、真理値(bool)同士にも使える *)
true = (1 = 2);;

(* 文字列は普通 *)
"abc" ;;

(* ただし、文字列に日本語を書くと、どうなるかは処理系依存 *)
"これは日本語です。あなたの処理系では読めますか?" ;;

(* 文字列の演算は多いが、とりあえず、2つの文字列の連接のみ *)
let x = "oo" in
  "abc" ^ x  ;;
let x = "oo" in
  "abc" ^ x ^ "def" ;;

(* 2つ以上のデータの組は (a,b) の形であらわす *)
(1, true) ;;
("abc", 2) ;;
("abc", 2, 3) ;;
(1,true) = (true,1) ;;

(* パターンマッチの専用構文がある *)
match true with
  true -> "abb"
| false -> "baa" ;;

(* _ アンダースコアは、場合分けにおける「default」を表す *)
let x = 1 in
  match x with
  0 -> "abb"
| 1 -> "baa"
| _ -> "unexpected" ;;  (* 上記全てにマッチしない時、このケース*)

(* 実は、組に対してもパターンマッチできる *)
let x = (1,true) in
  match x with
  (0,false) -> "a"
| (0,true) -> "b"
| (1,false) -> "c"
| (1,true) -> "d"
| _ -> "unexpected" ;;

(* パターンの中に、変数を書くことができる *)
let x = (1,true) in
  match x with
  (0,false) -> 0
| (0,true) ->  1
| (x,true) ->  x + 2   (* パターンマッチで得た x を右辺で使う *)
| _ -> -1 ;;

(* データ型を定義する *)
type white_or_black =   (* データ型の名前は小文字で始まる *)
    White of int        (* データ型の構成子は大文字で始まる *)
  | Black of string ;;
White 1;;
Black "abc";;

(* データ型に対するパターンマッチができる *)
let x = White 1 in
   match x with
     White x -> x
   | Black y -> 0 ;;  (*どのケースでも同じ型の値を返す *)

(* もし、ケースが不足していると、実行可能であっても警告がでる。 *)
let x = White 1 in
   match x with
     White x -> x ;;

(* failwith 文字列  という形で例外を置こすこともできる。
 * この時は exception という宣言はいらない。
 *)  
let f x y = 
            if y = 0 then failwith "unexpected input"
            else x / y 
in f 3 2 ;;

(* failwith 以外の例外機構を使うには、まず新しい例外を宣言する *)
exception UnexpectedInput ;;

(* 次に、任意の式の一部で、例外を呼びだす *)
let f x y = 
            if y = 0 then raise UnexpectedInput
            else x / y 
in f 3 2 ;;

let f x y = 
            if y = 0 then raise UnexpectedInput
            else x / y 
in f 3 0 ;;


(* 例外呼び出しはどこに置いても型エラーは起きない *)
let f x y = 
            if y = 0 then (raise UnexpectedInput) + 1
            else x / y 
in f 3 0 ;;

(* 存在しない例外を呼ぶと、エラーになる *)
let f x y = 
            if y = 0 then (raise NoSuchException) + 1
            else x / y 
in f 3 0 ;;

(* 発生した例外を「つかまえる」ことができる *)
try 
  let f x y = 
    if y = 0 then (raise UnexpectedInput) + 1
            else x / y 
  in "answer is " ^ (string_of_int (f 3 0))
with
    UnexpectedInput -> "try to divide by zero" ;;



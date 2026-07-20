(* 例題ファイル3 *)
(* 型の話 *)

(* 基本的な型 *)
1 ;;
true ;;
"abc" ;;
1.0 ;;

(* 組の型 *)
(1, true, "abc");;

(* 関数の型 *)
let f1 x = x + 1;;
let f2 = fun x -> x + 1;;

let g1 x y = x + y + 1 ;;
let g2 x = fun y -> x + y + 1 ;;
let g3 = fun x -> fun y -> x + y + 1 ;;

(* 型の整合性をチェックしてから、プログラムを実行する 
   つまり、型が合わないと、プログラムは一切実行されない *)
(print_string "here!") + (1 + 2 + "abc") ;;

(* どんな型でもいいときは、型変数 'aを使う *)
let f x = x ;;
let g x y = (x,y) ;;
let h f x = f (f x) ;;

(* 本実験では使わないが、リストの要素すべてに
  関数を適用したいときに便利な関数 *)

List.map (fun x -> x + 1) [1; 2; 3] ;;

(* 型の整合性を気にするので、プリントするためには、
   型を合わせる必要がある *)

let s1 = "Is it " in
let s2 = (string_of_bool true) in
let s3 = " if " in
let s4 = (string_of_int 100) in
let s5 = " is a number ?" in
  print_string (s1 ^ s2 ^ s3 ^ s4 ^ s5);;





  

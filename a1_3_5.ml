let rec qsort lst =
  match lst with
    [] -> []
  | x :: l ->
    let left, right = List.partition (fun y -> (y < x)) l in
      (qsort left) @ (pvot :: qsort right)
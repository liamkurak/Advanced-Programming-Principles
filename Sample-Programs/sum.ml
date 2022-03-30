let rec sum (lst: int list) : int = 
  match lst with
  | [] -> 0
  | x::xs -> x + sum xs

let _ = 
  assert (sum [] = 0);
  assert (sum [1;2;3] = 6);
  assert (sum [1] = 1)

let rec product (lst: int list) : int = 
  match lst with
  | [] -> 1
  | x::xs -> x * product xs

let _ =
  assert (product [1;2;3] = product [] * product [1;2;3]);
  assert (product [1;2;3] = product [1] * product [2;3]);
  assert (product [1;2;3] = product [1;2] * product [3])


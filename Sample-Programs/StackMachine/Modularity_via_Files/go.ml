
open Expr

let e1 : expr =
  (* (4 / 2) + (2 * (5 - 1)) *)
  Add (Div (Num 4, Num 2), Mul (Num 2, (Sub (Num 5, Num 1))))

let result_eval = eval e1
let result_compile = run (compile e1)

let _ = 
  print_endline ("Eval: " ^ string_of_int result_eval);
  print_endline ("Compile: " ^ string_of_int result_compile)

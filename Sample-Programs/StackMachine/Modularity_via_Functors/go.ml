
open Expr

module EL = ExprF ( ListStack.ListStackM )
module EC = ExprF ( CustomStack.CustomStackM )

(* In class exercise..

   How does the use of functors help in the reuse of code? 
   Beyond what you've seen before in other languages?
*)

let _ =
  let open EL
  in
  let e1 : expr =
    (* (4 / 2) + (2 * (5 - 1)) *)
    Add (Div (Num 4, Num 2), Mul (Num 2, (Sub (Num 5, Num 1))))
  in
  let result_eval = eval e1
  in
  let result_compile = run (compile e1)
  in
    print_endline ("Using lists!\n") ;
    print_endline ("Eval: " ^ string_of_int result_eval);
    print_endline ("Compile: " ^ string_of_int result_compile)

let _ =
  let open EC
  in
  let e1 : expr =
    (* (4 / 2) + (2 * (5 - 1)) *)
    Add (Div (Num 4, Num 2), Mul (Num 2, (Sub (Num 5, Num 1))))
  in
  let result_eval = eval e1
  in
  let result_compile = run (compile e1)
  in
    print_endline ("Using custom stack!\n") ;
    print_endline ("Eval: " ^ string_of_int result_eval);
    print_endline ("Compile: " ^ string_of_int result_compile)

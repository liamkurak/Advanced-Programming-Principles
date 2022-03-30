(* A representation of expressions with integer and
   Boolean operators and values.

   This representation is problematic as soon as we add
   let-expressions, as discussed in the slides.
 *)

type int_expr =
  | Num of int
  | Add of int_expr * int_expr  
  | Sub of int_expr * int_expr
  | Mul of int_expr * int_expr
  | Div of int_expr * int_expr
  | IfTE of bool_expr * int_expr * int_expr

and  bool_expr =
  | True
  | False
  | Lt of int_expr * int_expr
  | Eq of int_expr * int_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
  | Not of bool_expr


let rec eval_int (e: int_expr) : int = match e with
  | Add (e1, e2) -> eval_int e1 + eval_int e2
  | Sub (e1, e2) -> eval_int e1 - eval_int e2
  | Mul (e1, e2) -> eval_int e1 * eval_int e2
  | Div (e1, e2) -> eval_int e1 / eval_int e2
  | Num n -> n
and eval_bool (e: bool_expr) : bool = match e with
  | True -> true
  | False -> false
  | Lt (e1, e2) -> eval_int e1 < eval_int e2

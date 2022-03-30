
open Arithmetic

module ExprF (S : Stack.StackS) : ArithmeticS = struct

type expr = Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr
          | Num of int

let rec eval (e: expr) : int =
  match e with
  | Num n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

(*  NumI 1; NumI 2; OpI (+)  --> 3
 *)

type instr = NumI of int
           | OpI of (int -> int -> int)

let rec compile (e:expr) : instr list = match e with
  | Num n -> [ NumI n ]
  | Add (e1, e2) -> compile e1 @ compile e2 @ [ OpI ( + ) ]
  | Sub (e1, e2) -> compile e1 @ compile e2 @ [ OpI ( - ) ]
  | Mul (e1, e2) -> compile e1 @ compile e2 @ [ OpI ( * ) ]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [ OpI ( / ) ]

let run (code: instr list) : int =
  let rec step (is: instr list) (stack: int S.t) : int =
    match is with
    | [] -> S.peek stack 
    | i::rest -> (match i with
                  | NumI n -> step rest (S.push n stack)
                  | OpI f -> let (v2, stack2) = S.pop stack in
                             let (v1, stack3) = S.pop stack2 in
                             step rest (S.push (f v1 v2) stack3)
                 )
  in step code S.empty

end


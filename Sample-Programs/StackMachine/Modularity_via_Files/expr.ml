
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
  let rec step (is: instr list) (stack: int ListStack.stack) : int =
    match is with
    | [] -> ListStack.peek stack 
    | i::rest -> (match i with
                  | NumI n -> step rest (ListStack.push n stack)
                  | OpI f -> let (v2, stack2) = ListStack.pop stack in
                             let (v1, stack3) = ListStack.pop stack2 in
                             step rest (ListStack.push (f v1 v2) stack3)
                 )
  in step code ListStack.empty

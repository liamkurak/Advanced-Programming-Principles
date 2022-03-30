
type expr = Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr
          | Num of int

let e1 : expr =
  (* (4 / 2) + (2 * (5 - 1)) *)
  Add (Div (Num 4, Num 2), Mul (Num 2, (Sub (Num 5, Num 1))))

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


let empty = []
let peek s = match s with
      | [] -> raise (Failure "empty stack")
      | top::_ -> top
let push v s = v :: s
let pop s = match s with
  | [] -> raise (Failure "empty stack")
  | top::rest -> (top, rest)


let run (code: instr list) : int =
  let rec step (is: instr list) (stack: int list) : int =
    match is with
    | [] -> peek stack 
    | i::rest -> (match i with
                  | NumI n -> step rest (push n stack)
                  | OpI f -> let (v2, stack2) = pop stack in
                             let (v1, stack3) = pop stack2 in
                             step rest (push (f v1 v2) stack3)
                 )
  in step code empty

let result_eval = eval e1
let result_compile = run (compile e1)

let _ = 
  print_endline ("Eval: " ^ string_of_int result_eval);
  print_endline ("Compile: " ^ string_of_int result_compile)

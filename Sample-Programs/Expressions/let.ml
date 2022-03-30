(* We now add let-expressions to our little language
   of expressions.
  *)

type expr = Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr
          | Num of int
          | Id of string
          | Let of string * expr * expr

let e1 = (* "let x = 5 in 4 + x" *)
  Let ("x", Num 5, Add (Num 4, Id "x"))

type environment = (string * int) list

let rec lookup (x: string) (env: environment) : int =
  match env with
  | [] -> raise (Failure ("name " ^ x ^ " is not declared"))
  | (name,value):: rest when x = name -> value
  | (name,value):: rest -> lookup x rest

let rec eval (e: expr) (env: environment) : int =
  match e with
  | Num v -> v
  | Add (e1, e2) -> eval e1 env + eval e2 env
  | Mul (e1, e2) -> eval e1 env * eval e2 env
  | Id x -> lookup x env
  | Let (x, e1, e2) -> 
     let binding : string * int = (x, eval e1 env) in
     eval e2 (binding :: env)

(* let x = 1 in
   let y = 2 in
   let z = 3 in
   [(x,1); (y,2), (z,3)]
   (let a = 4 in a + x)  + (let a = 5 in a + y)
     [(a,4);(x,1); (y,2), (z,3)]         [(a,5);(x,1); (y,2), (z,3)]

*)

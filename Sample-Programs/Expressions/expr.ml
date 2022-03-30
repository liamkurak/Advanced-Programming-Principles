type expr = Add of expr * expr
          | Sub of expr * expr
          | Mul of expr * expr
          | Div of expr * expr
          | Num of int

let e1 : expr =   (*  (4 / 2) + (2 * (5 - 1)) *)
  Add (Div (Num 4, Num 2), Mul (Num 2, Sub (Num 5, Num 1)))

let rec string_of_expr (e: expr) : string = 
 match e with
 | Num v -> Int.to_string v
 | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ "+" ^ string_of_expr e2 ^ ")"
 | Sub (e1, e2) -> "(" ^ string_of_expr e1 ^ "-" ^ string_of_expr e2 ^ ")"
 | Mul (e1, e2) -> "(" ^ string_of_expr e1 ^ "*" ^ string_of_expr e2 ^ ")"
 | Div (e1, e2) -> "(" ^ string_of_expr e1 ^ "/" ^ string_of_expr e2 ^ ")"

let rec eval (e: expr) : int = match e with
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2
  | Num n -> n



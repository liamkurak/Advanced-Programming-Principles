
module type ArithmeticS = sig

  type expr = Add of expr * expr
            | Sub of expr * expr
            | Mul of expr * expr
            | Div of expr * expr
            | Num of int

  val eval : expr -> int

  type instr 

  val compile : expr -> instr list

  val run : instr list -> int

end

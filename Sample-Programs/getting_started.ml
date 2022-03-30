
let square (n: int) : int = n * n

let inc x = x + 1

let msg : string = "Hello!"

let dec : int -> int = fun x -> x - 1

let circle_area (r: float) : float =
  let pi = 3.14159265358979 
  in
  r *. r *. pi

(* Two equivalent versions of add. *)
let add (x: int) (y: int) : int = x + y
let add' = fun (x:int) -> (fun (y:int) -> x + y)


let rec fac n = 
  if n = 0 then 1 else n * fac (n-1)

let x = 7

let rec sum_to (n: int) : int =
  if n < 0 
  then raise (Failure "sum_to given a negative number")
  else
  if n = 0 then 0 else n + sum_to (n-1)

(* Evaluation of the expressions sum_to 3
  sum_to 3
  3 + sum_to (3-1)
  3 + sum_to 2
  3 + (2 + sum_to 1)
  3 + (2 + (1 + sum_to 0))
  3 + (2 + (1 + 0))
  3 + (2 + 1)
  3 + 3
  6 *)


(* This version more closely matches the evaluation above. *)
let rec sum_to' (n: int) : int =
  match n with
  | _ when n < 0 -> raise (Failure "sum_to given a negative number")
  | 0 -> 0
  | _ -> n + sum_to' (n-1)

(* an odd function demostrating that if-then-else is
   just an expression in OCaml. It evaluates to some
   value just like other expressions.
 *)
let rec sum_to_100 (n: int) : int =
  (if n = 0 then 0 else n + sum_to (n-1)) + 100

let rec power (n: int) (x: float) : float =
  if n = 0 then 1.0 else x *. power (n-1) x

let cube = power 3


let gcd (x: int) (y: int) : int =
  let m = if x < y then x else y
  in
  let rec decrement (i: int) : int =
    if x mod i = 0 && y mod i = 0
    then i
    else decrement (i - 1)
  in
  decrement m

(* An accumulating parameter example. Here, sum is the
   accumulating parameter to adder.
 *)
let sum_to_accum_up (n: int) :int  =
  let rec adder (sum: int) (i: int): int =
    if i > n
    then sum
    else adder (sum + i) (i + 1)
  in
  adder 0 0

(* Evaluation of adder 0 0 when n = 3
   adder 0 0
 = adder (0+0) (0+1)
 = adder 0 1
 = adder 1 2
 = adder 3 3
 = adder 6 4
 *)




let is_square (n: int) : bool =
  let rec check (i: int)  : bool =
    if i * i = n
    then true
    else
    if i * i < n 
    then check (i+1)
    else false
  in
  check 0

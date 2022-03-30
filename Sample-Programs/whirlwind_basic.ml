(* A function to square numbers *)

let square (x: int) : int = x * x


(* A function to increment an integer *)

let inc x = x + 1


(* Summing numbers from 0 up to `n` *)

let rec sum_to (n: int) : int =
  if n = 0
  then 0 
  else n + sum_to (n-1)


(* Fibonacci *)
let rec fib (x: int) :int  =
  if x = 0 
  then 0 
  else if x < 3 
  then 1 
  else fib (x-1) + fib (x-2)

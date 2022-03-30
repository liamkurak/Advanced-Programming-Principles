(* list and tuple examples from S1.2 *)


(* 
We can think of the `list` type as being defined as follows,
even though this isn't quite valid OCaml code and the list
type is built in.

But it has 2 constructors, the :: constructor being the interesting
inductive one.

type 'a list = []
             | :: of 'a * 'a list

                      ^    ^
                      |    |
       this is the head    this is the tail
*)

let rec sum (nums: int list) : int =
  match nums with
  | [] -> 0
  | x::xs -> x + sum xs

(* Evaluate: sum (1::2::3::[])
  sum (1::2::3::[])
= 1 + sum (2::3::[])
= 1 + (2 + sum (3::[]))
= 1 + (2 + (3 + sum []))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
*)

let rec length (lst: 'a list) : int =
  match lst with
  | [] -> 0
  | x::xs -> 1 + length xs

let rec all (bools: bool list) : bool =
  match bools with
  | [] -> true
  | b::bs -> b && all bs 

let rec num_evens (nums: int list) : int =
  match nums with
  | [] -> 0
  | x::xs -> if x mod 2 = 0 
             then 1 + num_evens xs
             else num_evens xs

let rec product (nums: int list) : int =
  match nums with
  | [] -> 1
  | x::xs -> x * product xs

let rec minimum (nums: int list) : int =
  match nums with
  | x::[] -> x
  | x::xs -> let m = minimum xs in
             if x < m then x else m
  | [] -> raise (Failure "oops")

(* let's rewrite `mininum` to avoid the extra check for [] *)

let even_length (xs: 'a list) : bool =
  List.length xs mod 2 = 0

let rec even_length' (xs: 'a list) : bool =
  match xs with
  | [] -> true
  | n::ns -> not (even_length' ns)

(* Add the evaluation sequence here. 
  el (1::2::3::[])
= not (el (2::3::[])
= not (not (el 3::[]))
= not (not (not (el [])))
= not (not (not true))
= not (not false)
= not true
= false
*)


let rec inc_all (nums: int list) : int list =
  match nums with
  | [] -> []
  | n::ns -> n + 1 :: inc_all ns

let rec all_evens (nums: int list) : int list = 
  match nums with
  | [] -> [] 
  | n::ns -> if n mod 2 = 0 
             then n :: all_evens ns
             else all_evens ns

let rec all_evens' (nums: int list) : int list = 
  match nums with
  | [] -> [] 
  | n::ns when n mod 2 = 0 -> n :: all_evens' ns
  | n::ns -> all_evens' ns


let even2ways (nums: int list) : bool =
  even_length nums &&
    nums = all_evens nums

let rec even2ways' (nums: int list) : bool =
  match nums with
  | [] -> true
  | n::[] -> false
  | n1::n2::ns -> n1 mod 2 = 0 && n2 mod 2 = 0 && even2ways ns
   

let rec concat (sep: string) (strs: string list) : string =
  match strs with
  | [] -> ""
  | [s] -> s
  | s::ss -> s ^ sep ^ concat sep ss


let is_empty (lst: 'a list) : bool =
  match lst with
  | [] -> true
  | _  -> false


let rec append (l1: 'a list) (l2: 'a list) : 'a list =
  match l1 with
  | [] -> l2
  | x::xs -> x :: (append xs l2)

let head (lst: 'a list) : 'a =
  match lst with
  | x::_ -> x
  | [] -> raise (Failure "head given an empty list.")

(*
let rec drop_value_incorrect (to_drop: 'a) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | to_drop :: tl -> drop_value_incorrect to_drop tl
  | hd :: tl -> hd :: drop_value_incorrect to_drop tl
*)
let rec drop_value (to_drop: 'a) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | hd :: tl when hd = to_drop -> 
     drop_value to_drop tl
  | hd :: tl ->   (* when hd <> to_drop *)
     hd :: drop_value to_drop tl

(* Pattern matchings gives us a certain level of 
   safety in writing functions in that it can prevent
   us from trying to access data that is not there.

   For example, consider the following function over
   `string list` values:
 *)
let first_string (ss: string list) : string =
  match ss with
  | [] -> "Oh no, not first string!"
  | s :: _ -> s

(* Compare the function above with the two below: 
 *)

let first_string_risky_v1 (ss: string list) : string =
  if is_empty ss
  then "Oh no, not first string!"
  else head ss

let first_string_risky_v2 (ss: string list) : string =
  if is_empty ss
  then head ss
  else "Oh no, not first string!"

(* Which one works and which one may raise an exception?
   
   Pattern matching, as seen in the original `first_string` 
   function is safer because we cannot make the mistake
   we see above of trying to get data out of value when
   the value does not contain the data that we seek.
 *)


(* Tuples
   ------
 *)

let add_pair (p: int * int) : int =
  match p with
  | (n1, n2) -> n1 + n2

let add_pair_v2 (p: int * int) : int =
  let (n1, n2) = p in  n1 + n2

let add_pair_v3 ((n1, n2) : int * int) : int =
  n1 + n2

let first_of_3 (t: 'a * 'b * 'c) : 'a =
  match t with
  | (a, _, _) -> a


let square_cube (n: int) : (int * int) =
  let sq = n * n in (sq, sq * n)

let add_square_cube (n: int) : int =
  let sc = square_cube n in
  match sc with
  | (s, c) -> s + c

let add_square_cube_shorter (n: int) : int =
  match square_cube n with
  | (s, c) -> s + c

let add_square_cube_shortest (n: int) : int =
  let (s, c) = square_cube n in s + c

let rec power (n: int) (x: float) : float =
  if n = 0 then 1.0
  else x *. power (n-1) x


let rec apply_all (f: float -> float) (ns: float list) : float list =
  match ns with
  | [] -> []
  | m::ms -> f m :: apply_all f ms


type fraction = int * int

let add (r1: fraction) (r2: fraction) : fraction =
  let (r1n, r1d) = r1 in
  let (r2n, r2d) = r2 in
  match (r1d, r2d) with
  | 0, _ -> raise (Failure "zero denominator")
  | _, 0 -> raise (Failure "zero denominator")
  | _ -> (r1n * r2d + r2n * r1d, r1d * r2d)

type ('a, 'b) dictionary = ('a * 'b) list

let rec lookup_all (m: ('a,'b) dictionary) (k: 'a) : 'b list = 
  match m with
  | [] -> []
  | (k', v) :: rest when k' = k -> v :: lookup_all rest k
  | (_, _) :: rest -> lookup_all rest k


let rec fib x =
  if x = 0 then 0 else
    if x = 1 then 1 else fib (x-1) + fib (x-2)

let rec fib' x = match x with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib' (x-1) + fib' (x-2)

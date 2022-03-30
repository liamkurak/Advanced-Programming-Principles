
type 'a lst = Nil
            | Cons of 'a * 'a lst
(*                     ^    ^
                       |    |
                the head    the tail
 *)


(* Consider the list 1, 2, 3, 4, 5. As a `int lst` value: *)

let l1 : int lst = 
  Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Nil)))))

(* We can draw this as follows: 

   1
    \
     2
      \
       3
        \
         4
          \
           5

The nodes have only 1 child.
*)

let rec sum (nums: int lst) : int =
  match nums with
  | Nil -> 0
  | Cons(n, ns) -> n + sum ns


(* Built in OCaml lists.  OCaml has lists build in and uses 
   the names
   []  for Nil
   ::  for Cons

   In fact, we pronouce [] as "Nil" and :: "Cons".
 *)

let l2 : int list = 1 :: 2 :: 3 :: 4 :: 5 :: []

let rec sum2 (nums: int list) : int =
  match nums with
  | [] -> 0
  | n::ns -> n + sum2 ns


(* OCaml has some syntactic sugar that makes writing lists
   a bit easier. But "under the hood" lists are constructed
   a deconstructed using :: and [].

   So l2 and l3 are exactly the same to OCaml.
 *)

let l3 = [1; 2; 3; 4; 5]

let is_true: bool =  l2 = l3

(* find and lookup examples *)

(* Recall lookup_all. We wrote a monomorphic version first
   and then generalized it to become polymorphic.  The first 
   version was as follows:
 *)

let rec lookup_all_string_int (m: (string * int) list) (k: string) : int list = 
  match m with
  | [] -> []
  | (k', v) :: rest when k' = k -> v :: lookup_all_string_int rest k
  | (_, _) :: rest -> lookup_all_string_int rest k

(* This function worked on dictionaries of string and integer pairs. *)
let m : (string * int) list = 
  [ ("dog", 1); ("chicken", 2); ("dog", 3); ("cat", 5) ] 

let _ = assert (lookup_all_string_int m "dog" = [1; 3])

(* We could have written another version for integer * char pairs.
   It would like this the following. Note that the body of the
   function is the same as `lookup_all_string_int` except for the
   function name!
 *)
let rec lookup_all_int_char (m: (int * char) list) (k: int) : char list = 
  match m with
  | [] -> []
  | (k', v) :: rest when k' = k -> v :: lookup_all_int_char rest k
  | (_, _) :: rest -> lookup_all_int_char rest k

let m2 : (int * char) list =
  [ (1, 'o'); (2, 't'); (1, 'n'); (2, 'w'); (1, 'e'); (2, 'o') ]

let _ = assert (lookup_all_int_char m2 1 = ['o'; 'n'; 'e'])

(* We then generalized it using parametric polymorphism so it could 
   work on dictionaries with different types.  Again, the function body
   is the same as the previous two, except for the function name.
*)
type ('a, 'b) dictionary = ('a * 'b) list

let rec lookup_all (m: ('a,'b) dictionary) (k: 'a) : 'b list = 
  match m with
  | [] -> []
  | (k', v) :: rest when k' = k -> v :: lookup_all rest k
  | (_, _) :: rest -> lookup_all rest k

(* We are relying in all of these on the polymorphic nature of 
   the = operator which has the type 'a -> 'a -> bool.
*)


let rec find_all_by (f: 'a -> bool) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | x::xs when f x -> x :: find_all_by f xs
  | x::xs -> find_all_by f xs

let even (x: int) : bool = x mod 2 = 0

let _ = 
  assert (find_all_by even [1;2;3;4] = [2;4]);
  assert (find_all_by even [2;4;6] = [2;4;6]);
  assert (find_all_by even [1;3;5] = []);
  assert (find_all_by even [] = [])


let big_nums (n: int) (nums: int list) : int list =
  find_all_by (fun m -> m > n) nums

let big_nums_v2 (n: int) (nums: int list) : int list =
  find_all_by ( (<=) n ) nums


let _ = 
  assert (find_all_by (fun x -> x mod 2 = 0) [1;2;3;4] = [2;4])

let _ = assert ( ( * ) 3 4 = 12)


let streq (s1: string) (s2: string) : bool = s1 = s2

let is_hello s = streq s "hello"

let find_all_hellos ss = find_all_by is_hello ss
let find_all_his ss = find_all_by (streq "hi")  ss

let big_strs (n: int) (ss: string list) : string list =
  find_all_by (fun s -> String.length s > n) ss



let rec merge (c: 'a -> 'a -> int) (l1: 'a list) (l2: 'a list) : 'a list =
  match (l1, l2) with
  | [], _ -> l2
  | _, [] -> l1
  |x::xs, y::ys -> if c x y = 0
                   then x :: merge c xs ys
                   else if c x y = 1
                   then y :: merge c (x::xs) ys
                   else x :: merge c xs (y::ys)

let merge_ascending (xs: 'a list) (ys: 'a list) : 'a list =
  merge compare xs ys

let merge_descending_v1 (xs: 'a list) (ys: 'a list) : 'a list =
  merge (fun x y -> -1 * compare x y) xs ys

let merge_descending_v2 (xs: 'a list) (ys: 'a list) : 'a list =
  merge (fun x y -> compare y x) xs ys

let flip f a b = f b a

let merge_descending_v3 (xs: 'a list) (ys: 'a list) : 'a list =
  merge (flip compare) xs ys


(* flip :  ('b -> 'a -> 'c)  -> 'a  -> 'b  -> 'c  *)

(* x < y ... y < x *)




let rec drop_while (f: 'a -> bool) (lst: 'a list) : 'a list =
  match lst with
  | [] -> [] 
  | x::xs when f x -> drop_while f xs
  | _ -> lst

let _ = assert (drop_while (fun x -> x > 6) [7;8;9;1;2;3;8;9] = [1;2;3;8;9])
let _ = assert (drop_while (fun x -> x > 6) [7;8;9] = [])




(* Higher order functions for common compuational patterns *)

let rec map (f: 'a -> 'b) (lst: 'a list) : 'b list =
  match lst with
  | [] -> []
  | x::xs -> f x :: map f xs


let get_excited (cs: char list) : char list =
  map (fun c -> if c = '.' then '!' else c) cs



let rec filter (f: 'a -> bool) (lst: 'a list) : 'a list =
  match lst with
  | [] -> []
  | x::xs when f x -> x :: filter f xs
  | _::xs -> filter f xs


let explode (s: string) : char list =
  let l = String.length s
  in
  let rec f i = 
    if i = l then [] else s.[i] :: f (i+1)
  in f 0


let smush (cs: char list) : char list =
  let f c = not ( c = ' ' || c = '\t' || c = '\n' )
  in
  filter f cs

let no_uppercase (cs: char list) : char list =
  let is_not_upper (c : char) : bool =
    let c_code = Char.code c in
    c_code < (Char.code 'A') || c_code > (Char.code 'Z')
  in
  filter is_not_upper cs



let rec foldr (f: 'a -> 'b -> 'b) (lst: 'a list) (v: 'b) : 'b = 
  match lst with
  | [] -> v
  | x::xs -> f x (foldr f xs v)

(* 
  foldr (+) (1::2::3::[]) 0
= (+) 1 (foldr (+) (2::3::[]) 0)
= 1 + (foldr (+) (2::3::[]) 0)
= 1 + ((+) 2 (foldr (+) (3::[]) 0))
= 1 + (2 + (foldr (+) (3::[]) 0))
= 1 + (2 + ((+) 3 foldr (+) [] 0)))
= 1 + (2 + (3 + (foldr (+) [] 0)))
= 1 + (2 + (3 + 0))
...
*)

let rec foldl (f: 'b -> 'a -> 'b) (acc: 'b) (lst: 'a list) : 'b =
  match lst with
  | [] -> acc
  | x::xs -> foldl f (f acc x) xs

let length lst = foldl (fun acc x -> acc + 1) 0 lst

(*
  foldl (+) 0 (1::2::3::[])
= foldl (+) (0+1) (2::3::[])
= foldl (+) 1 (2::3::[])
= foldl (+) (1+2) (3::[])
= foldl (+) 3 (3::[])
= foldl (+) (3+3) []
= foldl (+) 6 []
= 6
*)

let maximum lst = match lst with
    | [] -> raise (Failure "oops")
    | x::xs -> foldl (fun acc y -> if acc > y then acc else y) x xs 

(* Can we write maximum so that it can demonstrate to new
   users of folds how it works?

   That is, can it be rewritten to explain how fold works to
   novices?
 *)

let maximum' (lst: 'a list) : 'a =
  let pick_max (current_max: 'a) (y: 'a) : 'a =
    if current_max > y then current_max else y
  in
  match lst with
  | [] -> raise (Failure "list should not be empty")
  | x::xs -> foldl pick_max x xs


let all_are_even (nums: int list) : bool =
  let check_even (n: int) (all_even_sofar: bool) : bool =
    n mod 2 = 0 && all_even_sofar
  in
  foldr check_even nums true

let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

let lebowski (phrase: char list) : char list =
  let dudify (c: char) (phrase_so_far: char list) : char list =
    match c with
    | '.' -> [','; ' '; 'd'; 'u'; 'd'; 'e'; '.'] @ phrase_so_far
    | _ -> c :: phrase_so_far
  in
  foldr dudify phrase []


let partition_fr (f: 'a -> bool) (lst: 'a list) : 'a list * 'a list =
  let choose (x: 'a) ( buckets : ('a list * 'a list)) : 
        ('a list * 'a list) = 
    match buckets with
    | (yess, nos) -> 
       if f x 
       then (x :: yess, nos)
       else (yess, x :: nos)
  in
  List.fold_right choose lst ([], [])

let group_by_3 (lst: 'a list) : 'a list list =
  let collect ( (current_group, previous_groups) : 
                  'a list * 'a list list) 
              (x: 'a)  : ('a list * 'a list list) =
    if List.length current_group < 3
    then (x :: current_group, previous_groups)
    else ([x], (List.rev current_group) :: previous_groups)
  in
  match List.fold_left collect ([], []) lst with
  | (left_overs, the_groups)
    -> List.rev ( left_overs :: the_groups )

let sum_even_positions (nums: int list) : int =
  let sum ( (is_even, sum_so_far) : bool * int ) (n: int) : bool * int =
    if is_even
    then (false, n + sum_so_far)
    else (true, sum_so_far)
  in
  let (_, result) = List.fold_left sum (true,0) nums in result


type 'a stack

(* comments would be good... *)

val peek : 'a stack -> 'a

val pop : 'a stack -> 'a * ('a stack)

val push : 'a -> 'a stack -> 'a stack

val empty : 'a stack

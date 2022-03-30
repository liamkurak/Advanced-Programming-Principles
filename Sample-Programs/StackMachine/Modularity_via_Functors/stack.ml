
module type StackS = sig

  type 'a t

  val peek : 'a t -> 'a

  val pop : 'a t -> 'a * ('a t)

  val push : 'a -> 'a t -> 'a t

  val empty : 'a t

end

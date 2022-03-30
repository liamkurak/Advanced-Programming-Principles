
let do_something = print_endline "Hello!"

let say_hello = 
  let _ = print_endline "Plese enter your name:"
  in
  let name = read_line ()
  in
  print_endline ("Hello " ^ name ^ "!")

let _ =
  print_endline "Plese enter your name:" ;
  let name = read_line ()
  in
  print_endline ("Hello " ^ name ^ "!")

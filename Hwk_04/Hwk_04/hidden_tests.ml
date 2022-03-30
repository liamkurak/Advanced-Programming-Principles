
open Tests
open Solution

module type Hidden_SolutionS = sig
  val hidden_words : string list -> (string * string * string) list
end

module Hidden_Tests (S: Hidden_SolutionS) = struct

  let sunday : string list = [ 
      "act"; "doldrums"; "drums"; "era"; "forth"; "fortieth"; "moribund"; 
      "mound"; "old"; "operatic"; "optic"; "practice"; "price"; "protrude"; 
      "prude"; "ran"; "relet"; "relevant"; "rib"; "rot"; "spa"; "tie"; 
      "trespass"; "tress"; "van"; "warranty"; "warty"
    ]

                           
  let some_tests = [

    eval_test (fun () ->
        List.mem ("act", "price", "practice") (S.hidden_words sunday), 
        true)
       "List.mem (\"act\", \"price\", \"practice\") (S.hidden_words sunday)"
        Bool.to_string;

    eval_test (fun () ->
        List.mem ("era", "optic", "operatic") (S.hidden_words sunday), 
        true)
       "List.mem (\"era\", \"optic\", \"operatic\") (S.hidden_words sunday)"
        Bool.to_string;

    eval_test (fun () ->
        List.length (S.hidden_words sunday), 9)
       "List.length (S.hidden_words sunday"
        Int.to_string

    ]

  let run () = run_tests some_tests

end

module Tests_List_Set = Hidden_Tests ( Hidden_Words_List_Set )

module Tests_Tree_Set = Hidden_Tests ( Hidden_Words_Tree_Set )

let _ =
  print_endline "Running tests for list set solution.";
  print_endline "------------------------------------";
  Tests_List_Set.run ();

  print_endline "\n\n";
  print_endline "Running tests for tree set solution.";
  print_endline "------------------------------------";
  Tests_Tree_Set.run ()


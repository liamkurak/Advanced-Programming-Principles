# Hwk 04: Modularity in OCaml

### Advanced Programming Principles, Spring 2022

**Due: Wednesday, March 30, 5:00pm.**

## Hidden Words

This project asks you to help Will Shortz find additional solutions to the NPR Sunday Puzzle game "Find the Hidden Word." The audio of the show on July 18, 2021 when this game was played and a description of the game can be found here:

- [https://www.npr.org/2021/07/18/1017339265/sunday-puzzle-find-the-hidden-word](https://www.npr.org/2021/07/18/1017339265/sunday-puzzle-find-the-hidden-word)

Do listen to the audio to understand how the game works.

Your job is the find additional solutions that could be used if this game was to be played again. To do this, you will write an OCaml program to try different combinations of 3 and 5 letter words from some sample word banks.

For example, in processing the work bank in `words-google-10000.txt` your program should find the words `row`, `thing`, and `throwing` in that file and discover that the 3 letter word `row` can be inserted into the 5 letter word `thing` to get the word `throwing`.

The rules for this game are that the 3 letter word must be inserted **inside** the 5 letter word, not just added to the beginning or end of the 5 letter word.  Thus, it is **not** correct take the 3 letter word `way` and the 5 letter word `broad` and produce `broadway`.  Similarlly, it is not correct to take `per` and `forms` to create `performs`.  Neither of these are inserting the 3 letter word **inside** of the 5 letter word and thus they are not answers to the puzzle.


## Words and word banks

Since we are only interested in 3, 5, and 8 letter words your code might filter the input word list to create 3 different lists of words: one of 3 letter words, one of 5 letter words, and one of 8 letter words.

The problem then boils down to inserting every 3 letter word into every position inside every 5 letter word to see if the result is in the list of 8 letter words.

### Representations for words.

Words can be represented in OCaml using the type `string` or the type `char list`.  The functions `implode` and `explode` given below convert between these representations.
```ocaml
let implode (cs: char list) : string =
  String.concat "" (List.map  (String.make 1) cs)

let explode (s: string) : char list =
  let l = String.length s in
  let rec f i = if i = l then [] else s.[i] :: f (i+1) in
  f 0
```

As we see in Lab 08, a convenient representation of a word and a position in that word is a tuple containing a prefix of the word and the remaining contents of the word. One way to represent this is with the type `char list * char list`, another is `string * string`.

You will need to determine how you will represent words in your solution.

For example, one could represent the word `thing` and the position in which the word `row` can be inserted to create `throwing` as the pair
```ocaml
( ['t'; 'h'], ['i'; 'n'; 'g'])
```


### Reading words from files

The function below may be useful in reading the words out of a file to create a `string list` of those words.
```ocaml
let read_words (file_name: string) : string list =
  let ic = open_in file_name in
  let rec read_lines ic = try
      let next_line = input_line ic in
      next_line :: read_lines ic
    with _ -> []
  in
  let raw_strings = read_lines ic 
  in
  List.filter (fun s -> String.length s > 0)
    (List.map String.trim raw_strings)
```

### Word banks

In the public repository in the directory `Course-Assignments/Homework/Files/Hwk_04` you will find several files containing lists of words that you may use in checking your work.

## Efficient word banks

As mentioned to above, one thing your OCaml program must do is look up words in a word bank. When a 3 letter word is inserted into a 5 letter word we must determine if the resulting 8 letter composition is a word in the given word bank.

For the smaller word banks, we will thus find few answers than a larger word bank since the mechanism used to determine if an 8 letter composition is a valid answer is to check to see that it is in the given word bank.

One representation of a word bank is a list. The process of checking if a candicate word is in the list can then be traversing the list looking for that word. This is a slow operation since it will take time linear in the size of the list.

Another representation would be a binary search tree like the one we
started to develop in `whirldwind_parametric_tree.ml` in the
`Sample-Programs` directory of the public repository.  In this file we
wrote a function `elem` for checking if a value is in a tree. This
operation takes time that is only logarithmic in the size of the word
bank - so this is much faster.

We did not write a function to insert a value into a `'a tree`. This operation should also only take logarithmic time and will create a new tree from the old one by adding the desired element.

Both lists and binary search trees can be viewed as a set for the purpose of this project.

In this project you will need to implement both a list-based set and a tree-based set.


## Modular development

We have spent some time in class discussing the OCaml module system and how to use signatures, modules, and funtors. Much of this was demonstrated in the `StackMachine` examples in the directory `Modularity_via_Functors`.

In this project you are asked to design your solution so that the code you write is done in a modular way. You will then need to explain your choices in choosing that design. Putting all the code in a single file is the opposite of modular design, so you will not want to do that. You should consider:

What OCaml signatures will you write?

What OCaml modules will you write?

What OCaml functors will your write?

Answering these questions will lead to a collection of OCaml files that form your solution.  You have quite a bit of freedom in deciding how you will do that.

In a file named `design.md` you should write (as a Markdown file) about your modular design, explaining how you answered the 3 questions above and your reasons for your answers.

## Getting Started

Create a ``Hwk_04`` directory in your individual course repository. This will be next to the other directories you've created such as ``Hwk_03``. 

There are several files in `Course-Assignments/Homework/Files/Hwk_04` in the public class repository that you'll want to copy into `Hwk_04`.  These are
- `sunday.txt` - a short word list using words from the puzzle on the radio
- `words-google-10000.txt` - a longer lists of words
- `words-google-10000-sorted.txt` - the same longer list, but now sorted.

The last two word banks come from this repository: [https://github.com/first20hours/google-10000-english](https://github.com/first20hours/google-10000-english)

Also copy the testing files `tests.ml` and `hidden_tests.ml`.


## A few requirements for testing

As described above, there are not many requirements for how you create a modular implementation for this problem.

But there are a few requirements to enable some testing, as described below.

Your code should be written so that the provided tests in `tests.ml` and `hidden_tests.ml` can compile (unchanged) using the following command:
```
ocamlbiuld hidden_tests.byte
```

For this to work you will need to create a file named `solution.ml` that defines two modules named

- `Hidden_Words_List_Set` and
- `Hidden_Words_Tree_Set`

that implement, respectively, solutions that use a set based on lists and binary search trees.  Each of these modules must implement a function named `hidden_words` with the following type:
```ocaml
string list -> (string * string * string) list
```
as seen in `hidden_tests.ml`.

## Writing transparent functions

In this assignment we will assess not only the behavior of the
functions you write but also how well they are written. For this
assignment this kind of assessment is still rather light and is
limited to the following requirements that your functions should
satisfy.

1. All functions (that is, those not nested inside of other
   function bodies in a `let`-expression) should have explicit type
   annotations given for all input arguments and for the output type
   of the function. 

2. The names of function arguments should be meaningful and give some
   indication of what they are to be used for.  For example, for a function that takes a word prefix and suffix as inputs, it would be wise to name these `prefix` and `suffix`.  Naming them `s1` and `s2` would be a rather poor way to name them since these names give no indication of how they are to be used.

3. Your pattern matching expressions (`match`) should not have redundant patterns.    
   This problem arises if your `match` has a pattern for empty lists, lists with one elements, and lists with at least one element.  Here, the pattern for lists with one element is probably redundant and is thus confusing to see in the `match` expression.

4. Your functions should be indented in a reasonable way. Here we are
   not looking for your functions to conform to any strict
   requirements, but only that they look reasonably nice.
   
5. Part of writing nice code is not writing it with lines that are way
   too long. So do not write any lines that are over 80 characters in
   length. This is a somewhat arbitrary limitation, but it does make
   code easier to read on smaller monitors and in most editor windows.

6. Your code should also avoid unnecessary computations and unruly
   code structure.  The goal here is to write code that is concise 
   and transparent.



## Using different word banks

There are 3 different word banks provided to you for this project and you may want to experiment with all of them. To do this, create two files:

- `hidden_list_set.ml` and
- `hidden_tree_set.ml`

and in each one add the following code:
```ocaml
open Solution

module M = Hidden_Words_????_Set

let _ =
  let args : string array = Sys.argv in
  if Array.length args < 2 
  then 
    print_endline "usage: `hidden_list_set.byte/native <word-bank-file>`"
  else
    let word_list : string list = ???.read_words args.(1)
    in
    let answers : (string * string * string) list = M.hidden_words word_list
    in
    ???.print_answers (List.sort compare answers)
```

Now change the `????` in the line `module M = ...` to be either `List` or `Tree` depending on the file so that the appropriate implmentation is used in each.

Notice that this code calls `read_words` (shown above) so you will need to replace the `???` before it with the name of a module in which it can be found.  Similarly, the `???` before `print_answers` must be changed so that this function can be used.  You may also add `open` declarations to this file to open up this module.

A definition of `print_answers` is below and you should place this in a reasonable place in the collection of modules that you are writing for this work.
```ocaml
let rec print_answers (ans: (string * string * string) list) =
  match ans with
  | [] -> ()
  | (w3,w5,w8) :: rest -> 
     print_endline ( "(" ^ w3 ^ ", " ^ w5 ^ ", " ^ w8 ^ ")" );
     print_answers rest
```

## Timing your solutions

Once you have `hidden_list_set.ml` and `hidden_tree_set.ml` working you can compile them as follows, either to native code or byte code:
```
 ocamlbuild hidden_list_set.native
```
or
```
 ocamlbuild hidden_tree_set.byte
```
Compiling to native code takes longer but the resulting code is faster. During testing and development it is often better to compile to byte code since the compilation is faster.


You can then try them out, for example
```
 ./hidden_list_set.byte sunday.txt
 ```
 or
 ```
 ./hidden_tree_set.native words-google-10000.txt
 ```

To collect timing information on Mac OS, Unix, and Linux systems you can use the `time` command to prefex any command you want to time.  For example,
```
time ./hidden_tree_set.native words-google-10000.txt
```
will print out the solutions followed by timing data like the following:
```
real  0m3.708s
user  0m3.376s
sys   0m0.022s
```

We will only concern ourselves with `real` time here.  In this case it is roughly 3.7 seconds.

Once all your solutions are working, run the following 4 commands:
```bash
time ./hidden_list_set.native words-google-10000.txt

time ./hidden_tree_set.native words-google-10000.txt

time ./hidden_list_set.native words-google-10000-sorted.txt

time ./hidden_tree_set.native words-google-10000-sorted.txt
```

Create a second Markdown file, this one called `Timing.md` and record the `real` time for each and explain the interesting similarities and differences and why they arise.




## Turning in your code.

You will have many OCaml files to submit, but only the following files
are specifically required:
- `design.md`
- `timing.md`
- `solution.ml`
- `hidden_list_set.ml`
- `hidden_tree_set.ml`

Make sure to submit these files, along with all other files that contain relevant code you have written for the assignment.

Before turning in your code, make sure that all the tests in the various testing files mentioned above do in fact pass.

To simplify turning in so many OCaml files, first run the following command:
```
 zip -r H.zip *.ml *.md
 ```

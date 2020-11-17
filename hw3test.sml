(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3provided.sml";
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]


val test2 = longest_string1 ["A","bc","cb", "C"] = "bc"

val test3 = longest_string2 ["A","bc","cb", "C"] = "cb"
 
val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test82 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME []


val test9a = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (TupleP [Variable "s", TupleP [Variable "t", Wildcard]]) = 1
val test9a3 = count_wildcards (TupleP [Variable "s", TupleP [Wildcard, Wildcard]]) = 2
val test9a4 = count_wildcards (ConstructorP ("SOME", TupleP [Variable "x", ConstP 3, Wildcard, Wildcard, Wildcard])) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths (TupleP [Variable "s", TupleP [Variable "t", Wildcard]]) = 3
val test9b3 = count_wild_and_variable_lengths (TupleP [Variable "s", TupleP [Wildcard, Wildcard]]) = 3
val test9b4 = count_wild_and_variable_lengths (ConstructorP ("SOME", TupleP [Variable "xyz", ConstP 3, Wildcard, Wildcard, Wildcard])) = 6

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("y", Variable("x")) = 0
val test9c2 = count_some_var ("s", (TupleP [Variable "s", TupleP [Variable "s", Wildcard]])) = 2
val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable "s", TupleP [Variable "t", Wildcard]]) = true
val test10b = check_pat (TupleP [Variable "t", TupleP [Variable "t", Wildcard]]) = false
val test10c = check_pat (ConstructorP ("SOME", TupleP [Variable "xyz", ConstP 3, Wildcard, Wildcard, Wildcard])) = true
val test10d = check_pat Wildcard = true

val test11a = match (Const(1), UnitP) = NONE
val test11b = match ((Tuple [Const 1, Tuple [Const 7, Unit]]),(TupleP [Variable "s", TupleP [Variable "t", Wildcard]])) = SOME [("s", Const 1), ("t", Const 7)]
val test11c = match ((Constructor ("SOME", Tuple [Unit, Const 3, Const 6, Const 9, Const 42])), (ConstructorP ("SOME", TupleP [Variable "xyz", ConstP 3, Wildcard, Wildcard, Wildcard]))) = SOME [("xyz", Unit)]
val test11d = match ((Constructor ("SONE", Tuple [Unit, Const 3, Const 6, Const 9, Const 42])), (ConstructorP ("SOME", TupleP [Variable "xyz", ConstP 3, Wildcard, Wildcard, Wildcard]))) = NONE
val test11e = match ((Constructor ("SOME", Tuple [Unit, Const 3, Const 6, Const 9, Const 42])), (ConstructorP ("SOME", TupleP [Variable "xyz", ConstP 3, Wildcard, Wildcard]))) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test12a = first_match Unit [ConstP 7] = NONE
val test12b = first_match Unit [ConstP 42, Wildcard] = SOME []
val test12c = first_match Unit [ConstP 42, (Variable "t"), Wildcard] = SOME [("t", Unit)]
val test12e = first_match Unit [ConstP 42, Wildcard,  (Variable "t"), Wildcard] = SOME []

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2provided.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_2 = all_except_option ("string", []) = NONE
val test1_3 = all_except_option ("string", ["string2", "str"]) = NONE
val test1_4 = all_except_option ("string", ["string2", "string", "str"]) = SOME ["string2", "str"]


val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1(
    [["Fred","Fredrick"],
     ["Elizabeth","Betty"],
     ["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test3_2 = get_substitutions2(
    [["Fred","Fredrick"],
     ["Elizabeth","Betty"],
     ["Freddie","Fred","F"]],
"Fred") = ["Fredrick","Freddie","F"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = remove_card ([(Hearts, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Ace)]
val test7_2 = remove_card ([(Hearts, Ace), (Clubs, Num 2)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 2)]
val test7_3 = remove_card ([(Hearts, Ace), (Clubs, Num 2)], (Clubs, Num 2), IllegalMove) = [(Hearts, Ace)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [] = true
val test8_2 = all_same_color [(Hearts, Ace), (Clubs, Num 9)] = false
val test8_3 = all_same_color [(Hearts, Ace), (Diamonds, King)] = true
val test8_4 = all_same_color [(Hearts, Ace), (Diamonds, King), (Clubs, Num 9)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [(Clubs, Num 2),(Diamonds, Ace)] = 13
val test9_2 = sum_cards [(Clubs, Queen),(Clubs, Ace)] = 21
val test9_3 = sum_cards [] = 0

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test10_2 = score ([(Hearts, Num 2),(Clubs, Ace)],10) = 9 
val test10_3 = score ([(Hearts, Num 2),(Diamonds, Ace)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

val test14 = officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42) = 3  

val test15 = officiate_challenge([(Clubs,Ace),(Diamonds,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        6) = 2  
             

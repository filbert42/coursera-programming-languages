(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (Char.isUpper o (fn x => String.sub(x, 0)))

val longest_string1 = foldl (fn (x, y) => if String.size x > String.size y then x else y) ""

val longest_string2 = foldl (fn (x, y) => if String.size x >= String.size y then x else y) ""

fun longest_string_helper f = foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) ""

val longest_string3 = longest_string_helper op>

val longest_string4 = longest_string_helper op>=

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o List.rev o explode

fun first_answer f lst = 
	case lst of
		[]    => raise NoAnswer
	  | x::xs => case f x of
				   SOME v => v
				 | NONE   => first_answer f xs

fun all_answers f xs =
	let 
		fun helper(xs', SOME acc) =
			case xs' of
				[]     => SOME acc
			  | x::xss => case f x of
			  				NONE   => NONE
				          | SOME x => helper(xss, SOME (acc@x))
	in
		helper(xs, SOME [])
	end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn var => String.size var) 

fun count_some_var(str, p) = g (fn _ => 0) (fn s => if s = str then 1 else 0) p

fun check_pat p =
	let 
	  fun get_all_vars p acc =
		case p of
	    	Variable x => [x]
	    	| TupleP ps => foldl (fn (p,i) => (get_all_vars p i)@i) [] ps
	    	| ConstructorP(_,p) => get_all_vars p acc
	    	| _ => []
	 
	  fun check_list lst = 
	  	case lst of 
	      [] => false 
	    | x::xs => (List.exists (fn y => y = x) xs) orelse (check_list xs)
	in
	   not (check_list (get_all_vars p []))
	end

fun match(v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
	  | (_, Variable s) => SOME [(s, v)]
	  | (Unit, UnitP) => SOME []
      | (Const i, ConstP i2) => if i = i2 then SOME [] else NONE
	  | (Tuple vps, TupleP pps) => if List.length pps = List.length vps
						  		   then (all_answers match (ListPair.zip(vps, pps)))
								   else NONE
	  | (Constructor (s, v2), ConstructorP (s2, p2)) => if s = s2 then match (v2, p2) else NONE
      | _ => NONE

fun first_match v lst = 
	let fun curry f x y = f (x, y)
	in SOME (first_answer (curry match v) lst) handle NoAnswer => NONE
	end
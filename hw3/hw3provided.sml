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
fun only_capitals xs =
    List.filter (fn x => (Char.isUpper(String.sub(x,0)))) xs

fun longest_string1 xs =
    List.foldl (fn (x,bigs) => if String.size(x) > String.size(bigs)
                               then x
                               else bigs) "" xs

fun longest_string2 xs =
    List.foldl (fn (x,bigs) => if String.size(x) >= String.size(bigs)
                               then x
                               else bigs) "" xs

fun longest_string_helper f xs =
    List.foldl (fn (x,bigs) => if f(String.size(x), String.size(bigs))
                               then x
                               else bigs) "" xs

val longest_string3 = longest_string_helper (op >)

val longest_string4 = longest_string_helper (op >=)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = implode o rev o explode

                 (* part 2 *)

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x::xs' => case f x of
                   SOME v => v
                 | NONE => first_answer f xs'

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

fun all_answers f xs =
    let fun helper f xs sofar =
    case xs of
           [] => sofar
         | x::xs' => helper f xs' (SOME ((valOf sofar)@[first_answer f x]))
    in helper f xs (SOME [])
    end

fun count_wildcards pat =
    g (fn () => 1) (fn (x) => 0) pat

fun count_wild_and_variable_lengths pat =
    g (fn () => 1) (fn (x) => String.size(x)) pat

fun count_some_var (s, pat) =
    g (fn () => 0) (fn (x) => if s = x then 1 else 0) pat

fun check_pat_trash pat =
    let fun helper pat seen = 
            case pat of
              Variable x => if List.exists (fn (y) => x = y) (valOf seen)
                              then NONE
                              else (SOME (x::(valOf seen)))
              | TupleP ps => (case ps of
                                 p::ps' => let val r = helper p seen
                                           in case r of
                                                  NONE => NONE
                                                | SOME (z::zs) => helper (TupleP ps') (SOME ((z::zs)@(valOf seen)))
                                                | SOME [] => helper (TupleP ps') seen
                                           end
                               | [] => seen)
              | _ => NONE
    in
        case helper pat (SOME []) of
            NONE => false
          | _ => true
    end

fun check_pat pat =
    let fun collect_var_names p sofar =
            case p of
	            Variable x        => x::sofar
	          | TupleP ps         => List.foldl (fn (pt,terms) => collect_var_names pt sofar@terms ) [] ps
	          | _                 => sofar
        fun is_unique sofar =
            case sofar of
                [] => true      (* FIXME: Sort list to make this linear time *)
              | x::xs => (not (List.exists (fn (y) => x = y) xs)) andalso is_unique xs
    in
        is_unique (collect_var_names pat [])
    end

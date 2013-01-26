(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s : string, xs: string list) =
    case xs of
        [] => NONE
      | x::xs' => if same_string(x, s) then SOME (xs')
                  else case all_except_option(s, xs') of
                           NONE => NONE
                         | SOME (z::zs) => SOME (x::(z::zs))
                         | SOME [] => SOME (x::[])

fun get_substitutions1(xs : string list list, s : string) =
    case xs of
        [] => []
      | x::xs' => case all_except_option(s, x) of
                     NONE => get_substitutions1(xs', s)
                   | SOME [] => get_substitutions1(xs',s)
                   | SOME (z::zs) => (z::zs)@get_substitutions1(xs',s)


fun get_substitutions2(xs : string list list, s : string) =
    let fun helper(xs : string list list, s : string, sofar : string list) =
            case xs of
                [] => sofar
              | x::xs' => case all_except_option(s, x) of
                              NONE => helper(xs', s, sofar)
                            | SOME [] => helper(xs', s, sofar)
                            | SOME (z::zs) => helper(xs',s, sofar@(z::zs))
    in helper(xs, s, [])
    end

fun similar_names(xs : string list list, fname : {first:string,middle:string,last:string} ) =
    let val {first=f, middle=m, last=l} = fname 
            fun helper(xs : string list, sofar : {first:string,middle:string,last:string} list) =
                case xs of
                    [] => {first=f, middle=m, last=l}::sofar
                  | x::xs' => helper(xs', {first=x, middle=m, last=l}::sofar)
    in
        helper(get_substitutions2(xs, f), [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(c : card) =
    case c of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black


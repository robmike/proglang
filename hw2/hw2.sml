(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s : string, xs: string list) =
    let 
        fun helper(s: string, xs: string list) =
            case xs of
                x::xs' => if same_string(x, s) then xs' else x::helper(s, xs')
              | [] => []
    in case helper(s, xs) of
            [] => NONE
          | x::xs' => SOME (x::xs')
    end

    (* case xs of *)
    (*     [] => NONE *)
    (*   | x::xs' => SOME x::all_except_option(s, xs') *)
    (*   | SOME x::xs' => SOME x::all_except_option(s, xs') *)
    (*   | s::xs' => SOME xs' *)
    (*   | SOME s::xs' => SOME xs' *)
    (*   | s::[] => s *)
    (*   | SOME s::[] => s *)
    (*   | _ => NONE *)

(* you may assume that Num is always used with values 2, 3, ..., 9
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

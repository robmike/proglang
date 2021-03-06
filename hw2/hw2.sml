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

fun card_value(c : card) =
    case c of
        (_, Num i) => i
      | (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11

fun remove_card(cs : card list, c : card, e) =
    case cs of
        [] => raise e
      | x::xs' => if x = c then xs'
                  else x::remove_card(xs', c, e)

fun all_same_color(xs : card list) =
    case xs of
        [] => true
        | x::[] => true
        | x::(y::xs') => (card_color(x) = card_color(y)) andalso all_same_color(y::xs')

fun sum_cards(xs : card list) =
    let fun helper(xs, total : int) =
            case xs of
                [] => total
              | x::xs' => helper(xs', card_value(x) + total)
    in
        helper(xs, 0)
    end

fun score(xs : card list, goal : int) =
    let fun prelim(xs : card list) =
        let val sum = sum_cards(xs)
        in
            if sum > goal then 3*(sum - goal) else goal - sum
        end
    in
        if all_same_color(xs) then prelim(xs) div 2 else prelim(xs)
    end

fun officiate(cs : card list, ms : move list, goal : int) =
    let
        fun playmove(cs : card list, hcs : card list, ms : move list) =
            case ms of
                [] => score(hcs, goal)
              | m::ms' => case m of
                              Discard z => playmove(cs, remove_card(hcs, z, IllegalMove), ms')
                            | Draw => case cs of
                                         [] => score(hcs, goal)
                                       | c::cs' => if score(c::hcs, goal) > goal
                                                  then score(c::hcs, goal)
                                                  else playmove(cs', c::hcs, ms')
    in
        playmove(cs, [], ms)
    end


(* val is_older = fn : (int * int * int) * (int * int * int) -> bool; *)
(* val number_in_month = fn : (int * int * int) list * int -> int; *)
(* val number_in_months = fn : (int * int * int) list * int list -> int; *)
(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list; *)
(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list; *)
(* val get_nth = fn : string list * int -> string; *)
(* val date_to_string = fn : int * int * int -> string; *)
(* val number_before_reaching_sum = fn : int * int list -> int; *)
(* val what_month = fn : int -> int; *)
(* val month_range = fn : int * int -> int list; *)
(* val oldest = fn : (int * int * int) list -> (int * int * int) option; *)

type date = (int * int * int);

fun is_older(d1 : date, d2 : date) =
    ((#1 d1) < (#1 d2)) orelse
    ((#1 d1) = (#1 d2)) andalso (((#2 d1) < (#2 d2)) orelse
                                 ((#2 d1) = (#2 d2)) andalso ((#3 d1) < (#3 d2)))

fun number_in_month(dates : date list, month : int) =
    foldl (fn(y : date, x : int) => if (month = (#2 y)) then x + 1 else x) 0 dates

fun number_in_months(dates : date list, months : int list) =
    foldl (fn(m : int, count : int) => number_in_month(dates, m) + count) 0 months

fun date_in_month(dates : date list, m : int) =
    rev(foldl (fn(d : date, out : date list) => if (m = (#2 d)) then d::out else out) [] dates)

fun date_in_months(dates : date list, months : int list) =
    foldl (fn(m : int, out : date list) => out@date_in_month(dates, m)) [] months

fun get_nth(strs : string list, n : int) =
    if (n = 1)
    then hd strs
    else get_nth(tl strs, n - 1)

fun date_to_string(d : date) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] 
    in
        get_nth(months, (#2 d)) ^ " " ^ Int.toString((#3 d)) ^ ", " ^ Int.toString((#1 d))
    end

fun number_before_reaching_sum(sum : int, xs : int list) =
    let
        val total = 0
        val count = 0
    in
        if total + (hd xs) >= sum
        then
            0
        else
            1 + number_before_reaching_sum(sum - (hd xs), (tl xs))
    end

fun what_month(dayofyear : int) =
    let
        val month_table = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(dayofyear, month_table)
    end

fun month_range(doy1 : int, doy2: int) =
    if doy2 < doy1
    then []
    else what_month(doy1) :: month_range(doy1 + 1, doy2)

fun oldest(xs: date list) =
    if null xs
    then NONE
    else
        let val tl_ans = oldest(tl xs)
        in if isSome tl_ans andalso is_older(valOf tl_ans, hd xs)
           then tl_ans
           else SOME (hd xs)
        end


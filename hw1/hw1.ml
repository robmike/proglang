val is_older = fn : (int * int * int) * (int * int * int) -> bool;
val number_in_month = fn : (int * int * int) list * int -> int;
val number_in_months = fn : (int * int * int) list * int list -> int;
val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list;
val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list;
val get_nth = fn : string list * int -> string;
val date_to_string = fn : int * int * int -> string;
val number_before_reaching_sum = fn : int * int list -> int;
val what_month = fn : int -> int;
val month_range = fn : int * int -> int list;
val oldest = fn : (int * int * int) list -> (int * int * int) option;

type date = (int * int * int);

fun is_older(d1 : date, d2 : date) =
    (* This is ugly *)
    ((#1 d1) < (#1 d2)) andalso ((#2 d1) < (#2 d2)) andalso ((#3 d1) < (#3 d2))

fun number_in_month(dates : date list, month : int) =
    foldl (fn(y : date, x : int) => if (month = (#2 y)) then x + 1 else x) 0 dates

fun number_in_months(dates : date list, months : int list) =
    foldl (fn(m : int, count : int) => if(number_in_month(dates, m) > 0) then count+1 else count) 0 months

fun date_in_month(dates : date list, m : int) =
    rev(foldl (fn(d : date, out : date list) => if (m = (#2 d)) then d::out else out) [] dates)

fun date_in_months(dates : date list, months : int list) =
    foldl (fn(m : int, out : date list) => out@date_in_month(dates, m)) [] months



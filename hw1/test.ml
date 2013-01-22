number_in_month([(1,2,3), (3,4,5), (5,5,5),(2,5,5), (2,4,5)], 9) = 0;
number_in_month([(1,2,3), (3,4,5), (5,5,5),(2,5,5), (2,4,5)], 5) = 2;
number_in_months([(1,2,3), (3,4,5), (5,5,5),(2,5,5), (2,4,5)], [9,2,5,4]) = 3;
number_in_months([(1,2,3), (3,4,5), (5,5,5),(2,5,5), (2,4,5)], [9,8]) = 0;
number_in_months([(1,2,3), (3,4,5), (5,5,5),(2,5,5), (2,4,5)], []) = 0

date_in_month([(1,2,3), (4,2,5), (1,1,1), (4,2,3)], 2) = [(4,2,3),(4,2,5),(1,2,3)];
date_in_month([(1,2,3), (4,2,5), (1,1,1), (4,2,3)], 1) = [(1,1,1)];
date_in_month([(1,2,3), (4,2,5), (1,1,1), (4,2,3)], 8) = [];

date_in_months([(1,2,3), (4,2,5), (1,1,1), (4,2,3)], [2,1,8]) = [(1,1,1), (4,2,3),(4,2,5),(1,2,3)]
date_in_months([(1,2,3), (4,2,5), (1,1,1), (4,2,3)], [9,8]) = []

get_nth(["a", "b", "c"], 1) = "a"
get_nth(["a", "b", "c"], 2) = "b"
get_nth(["a", "b", "c"], 3) = "c"

date_to_string((2013, 1, 5)) = "January 5, 2013"
date_to_string((2013, 11, 5)) = "November 5, 2013"
date_to_string((2013, 12, 5)) = "December 5, 2013"

number_before_reaching_sum(1, [1,2,4]) = 0
number_before_reaching_sum(3, [1,2,4]) = 1
number_before_reaching_sum(4, [1,2,4]) = 2
number_before_reaching_sum(6, [1,2,4]) = 2
number_before_reaching_sum(7, [1,2,4]) = 2

what_month(1) = 1
what_month(5) = 1
what_month(365) = 12
what_month(364) = 12
what_month(123) = 5
what_month(234) = 8

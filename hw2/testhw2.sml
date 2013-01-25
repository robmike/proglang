all_except_option("hello", ["hello", "two", "goodbye"]) = SOME ["two","goodbye"]
all_except_option("hello", ["one", "two", "goodbye", "hello"]) = SOME ["one","two","goodbye"]
all_except_option("hello", ["one", "two", "hello", "goodbye"]) = SOME ["one","two","goodbye"]
all_except_option("hello", ["one", "two" , "goodbye"]) = NONE

get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]


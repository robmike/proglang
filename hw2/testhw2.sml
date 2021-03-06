all_except_option("hello", ["hello", "two", "goodbye"]) = SOME ["two","goodbye"]
all_except_option("hello", ["one", "two", "goodbye", "hello"]) = SOME ["one","two","goodbye"]
all_except_option("hello", ["one", "two", "hello", "goodbye"]) = SOME ["one","two","goodbye"]
all_except_option("hello", ["one", "two" , "goodbye"]) = NONE

get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]

similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred",last="Smith",middle="W"},{first="F",last="Smith",middle="W"}, {first="Freddie",last="Smith",middle="W"}, {first="Fredrick",last="Smith",middle="W"}]

card_color((Clubs, Jack)) = Black
card_color((Diamonds, Jack)) = Red

card_value((Hearts, Num 5)) = 5
card_value((Hearts, Jack)) = 10

remove_card([(Hearts, Jack), (Spades, Num 10), (Spades, Ace)], (Spades, Num 10), IllegalMove)= [(Hearts,Jack),(Spades,Ace)]
(remove_card([(Hearts, Jack), (Spades, Num 10), (Spades, Ace)], (Spades, Num 9), IllegalMove) = [(Diamonds, Ace)]) handle IllegalMove => (1 = 1) (* Should return true and not exception *)

all_same_color([(Hearts, Jack), (Spades, Num 10), (Spades, Ace)]) = false
all_same_color([(Clubs, Jack), (Spades, Num 10), (Spades, Ace)]) = true

sum_cards([(Clubs, Jack), (Spades, Num 4), (Spades, Ace)]) = 25

score([(Clubs, Jack), (Spades, Num 4), (Diamonds, Ace)], 25) = 0
score([(Clubs, Jack), (Spades, Num 4), (Diamonds, Ace)], 35) = 10
score([(Clubs, Jack), (Spades, Num 4), (Diamonds, Ace)], 15) = 30
score([(Clubs, Jack), (Spades, Num 4), (Clubs, Ace)], 15) = 15
score([(Clubs, Jack), (Spades, Num 4), (Clubs, Ace)], 35) = 5

officiate([(Clubs, Ace)], [Draw], 5) = 9;
officiate([(Clubs, Ace)], [Discard (Clubs, Ace)], 5) handle IllegalMove => 0
officiate([(Clubs, Ace)], [Draw, (Discard (Clubs, Ace))], 5) = 2;
officiate([(Clubs, Num 2), (Hearts, Jack), (Hearts, King), (Spades, Num 10)], [Draw, Draw, Draw, (Discard (Clubs, Ace))], 5) = 21;
officiate([(Clubs, Num 2), (Hearts, Jack), (Hearts, King), (Spades, Num 10)], [Draw, Draw, Draw, (Discard (Clubs, Num 2))], 25) = 2;

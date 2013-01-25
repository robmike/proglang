all_except_option("hello", ["hello", "two", "goodbye"]) = SOME ["two","goodbye"]
all_except_option("hello", ["one", "two", "goodbye", "hello"]) = SOME ["one","two","goodbye"]
all_except_option("hello", ["one", "two", "hello", "goodbye"]) = SOME ["one","two","goodbye"]
all_except_option("hello", ["one", "two" , "goodbye"]) = NONE

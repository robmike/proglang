val only_capitals = fn : string list -> string list
val it = () : unit
- only_capitals(["blah", "foo", "Okay", "Then"])
= ;
val it = ["Okay","Then"] : string list
- [opening /tmp/sml3047RHP]
val longest_string = fn : string list -> string
val it = () : unit
- longest_string(["blah", "foobarqux", "Okay", "Then"])
= ;
val it = "foobarqux" : string
- longest_string(["blah", "foo", "Okay", "Then"])
= ;
val it = "blah" : string
- 
first_answer (fn x => x) [NONE, NONE, SOME 4];
val it = 4 : int
- first_answer (fn x => x) [NONE, NONE, SOME 4];

- all_answers (fn (x) => x) [[NONE, SOME 3]];
val it = SOME [3] : int list option
- all_answers (fn (x) => x) [[NONE, NONE, SOME 4], [NONE, SOME 3]];
val it = SOME [4,3] : int list option
- all_answers (fn (x) => x) [];
stdIn:17.1-17.29 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val it = SOME [] : ?.X1 list option

count_wildcards (TupleP ([Wildcard, UnitP, Wildcard, (TupleP [Variable "foo", Variable "bar", Wildcard, ConstP 4])]));

count_wild_and_variable_lengths (TupleP ([Wildcard, UnitP, Wildcard, (TupleP [Variable "foo", Variable "bart", Wildcard, ConstP 4])]));

count_some_var ("foo", (TupleP ([Wildcard, UnitP, Wildcard, (TupleP [Variable "foo", Variable "foo", Wildcard, ConstP 4])])));

check_pat (TupleP ([Wildcard, UnitP, Wildcard, (TupleP [Variable "foo", Variable "bat", Wildcard, Variable "foot"])]));
val it = true : bool
- check_pat (TupleP ([Wildcard, UnitP, Wildcard, (TupleP [Variable "foo", Variable "bat", Wildcard, Variable "foo"])]));
val it = false : bool


match (Tuple [Tuple [Unit], Const 4], TupleP [TupleP[Variable "foo"], ConstP 4]);
val it = SOME [("foo",Unit)] : (string * valu) list option
- match (Tuple [Tuple [Unit], Unit], TupleP [TupleP[Variable "foo"], Variable "bar"]);
val it = SOME [("foo",Unit),("bar",Unit)] : (string * valu) list option
- match (Tuple [Tuple [Unit], Const 4], TupleP [TupleP[Variable "foo"], Variable "bar"]);
val it = SOME [("foo",Unit),("bar",Const 4)] : (string * valu) list option
- match (Tuple [Tuple [Const 4], Const 4], TupleP [TupleP[ConstP 5], Variable "bar"]);
val it = NONE : (string * valu) list option
- 

first_match (Const 4) [UnitP, UnitP];
val it = NONE : (string * valu) list option
- first_match (Const 4) [UnitP, Variable "foo"];
val it = SOME [("foo",Const 4)] : (string * valu) list option
- 


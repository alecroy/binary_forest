-module(binary_forest_tests).
-import(binary_forest,
	    [create/0, create/1, cons/2, is_empty/1, head/1, tail/1, foreach/2,
         map/2, nth/2, update/3]).
-include("binary_forest.hrl").
-include_lib("eunit/include/eunit.hrl").


create_test() ->
    #forest{size=0, trees=[]} = create(),
    #forest{size=1, trees=[#tree{size=1, value=1}]} = create([1]),
    OneTwo = #forest{size=2,
                     trees=[#tree{size=2,
                                  left=#tree{size=1, value=1},
                                  right=#tree{size=1, value=2}}]},
    OneTwo = create([1, 2]),
    [T12] = OneTwo#forest.trees,
    #forest{size=4, trees=[#tree{size=4, left=T12, right=T12}]}
        = create([1, 2, 1, 2]),
    ok.


cons_test() ->
    One = #forest{size=1, trees=[#tree{size=1, value=1}]},
    One = cons(1, create()),
    TwoThree = #forest{size=2, trees=[#tree{size=2,
                                            left=#tree{size=1, value=2},
                                            right=#tree{size=1, value=3}}]},
    {[T1], [T23]} = {One#forest.trees, TwoThree#forest.trees},
    #forest{size=3, trees=[T1, T23]} = cons(1, cons(2, create([3]))),
    ok.


is_empty_test() ->
    true = is_empty(create()),
    false = is_empty(create([1])),
    ok.


tail_test() ->
    #forest{size=0, trees=[]} = tail(create([1])),
    #forest{size=1, trees=[#tree{size=1, value=2}]}
        = tail(create([1, 2])),
    ok.


head_test() ->
    1 = head(create([1])),
    3 = head(tail(tail(create([1, 2, 3])))),
    ok.


map_test() ->
    Identity = fun (X) -> X end,
    Forest = create(lists:seq(1, 5)),
    Forest = map(Identity, Forest),
    Squares = create([1, 4, 9, 16, 25]),
    Squares = map(fun (N) -> N * N end, Forest),
    ok.


nth_test() ->
    1 = nth(1, create([1])),
    100 = nth(100, create(lists:seq(1, 100))),
    ok.


update_test() ->
    Original = create([1, 2, 3]),
    Modified = create([1, 7, 3]),
    Modified = update(2, 7, Original),
    ok.

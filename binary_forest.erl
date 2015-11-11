-module(binary_forest).
-export([create/0, create/1, cons/2, is_empty/1, head/1, tail/1, foreach/2,
         map/2, nth/2]).
-include("binary_forest.hrl").
-include_lib("eunit/include/eunit.hrl").



create() -> create([]).
create(List) ->
    Size = length(List),
    #forest{size=Size, trees=trees(Size, List, [])}.


cons(Element, Forest) ->
    NewSize = Forest#forest.size + 1,
    Tree = #tree{size=1, values=[Element]},
    #forest{size=NewSize, trees=merge(Tree, Forest#forest.trees)}.


is_empty(Forest) -> Forest#forest.size == 0.


head(#forest{trees=[#tree{values=[Value | _Values]} | _Trees]}) -> Value.


tail(Forest = #forest{trees=[]}) -> Forest;
tail(#forest{size=Size, trees=[#tree{values=[_Value | Values]} | Trees]}) ->
    SubForest = create(Values),
    #forest{size=Size - 1, trees=SubForest#forest.trees ++ Trees}.


foreach(Function, #forest{trees=Trees}) ->
    lists:foreach(fun (Tree) -> foreach_tree(Function, Tree) end, Trees).

foreach_tree(Function, #tree{values=Values}) ->
    lists:foreach(Function, Values).


map(Function, Forest = #forest{trees=Trees}) ->
    NewTrees = lists:map(fun (Tree) -> map_tree(Function, Tree) end, Trees),
    Forest#forest{trees=NewTrees}.

map_tree(Function, Tree = #tree{values=Values}) ->
    NewValues = lists:map(Function, Values),
    Tree#tree{values=NewValues}.

nth(N, Forest) -> nth_trees(N, Forest#forest.trees).



%%% helpers

highest_power_of_2(N) -> trunc(math:pow(2, trunc(math:log2(N)))).


%% Split a List of length Size into trees each of size 2^k.
trees(_Length, [], Trees) -> Trees;
trees(Length, List, Trees) ->
    TreeSize = highest_power_of_2(Length),
    {Rest, Values} = lists:split(Length - TreeSize, List), % take values off end
    Tree = #tree{size=TreeSize, values=Values},
    trees(Length - TreeSize, Rest, [Tree | Trees]).


%% Merge a Tree with other Trees of size 2^k (like adding 1 to a binary number)
merge(Tree, []) -> [Tree];
merge(Tree, [T | Trees]) when Tree#tree.size < T#tree.size ->
    [Tree] ++ [T | Trees];
merge(Tree, [T | Trees]) ->
    Size = 2 * Tree#tree.size,
    Values = Tree#tree.values ++ T#tree.values,
    merge(#tree{size=Size, values=Values}, Trees).


nth_trees(N, [#tree{size=Size} | Trees]) when N > Size ->
    nth_trees(N - Size, Trees).
nth_trees(N, [#tree{size=Size, values=Values} | _Trees]) ->
    lists:nth(N, Values);



%%% tests

create_test() ->
    #forest{size=0, trees=[]} = create(),
    #forest{size=1, trees=[#tree{size=1, values=[1]}]} = create([1]),
    #forest{size=2, trees=[#tree{size=2, values=[1, 2]}]} = create([1, 2]),
    #forest{size=6, trees=[#tree{size=2, values=[1, 2]},
                           #tree{size=4, values=[3, 4, 5, 6]}
                           ]} = create(lists:seq(1, 6)),
    ok.


highest_power_of_2_test() ->
    16 = highest_power_of_2(17),
    16 = highest_power_of_2(16),
    8 = highest_power_of_2(15),
    ok.


cons_test() ->
    #forest{size=1, trees=[#tree{size=1, values=[1]}]} = cons(1, create()),
    Three = #forest{size=3, trees=[#tree{size=1, values=[1]},
                                   #tree{size=2, values=[2, 3]}]},
    Three = cons(1, cons(2, cons(3, create()))),
    ok.


is_empty_test() ->
    true = is_empty(create()),
    false = is_empty(create([1])),
    ok.


tail_test() ->
    #forest{size=0, trees=[]} = tail(create([1])),
    #forest{size=1, trees=[#tree{size=1, values=[2]}]} = tail(create([1, 2])),
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

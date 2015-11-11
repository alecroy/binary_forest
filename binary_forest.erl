-module(binary_forest).
-export([create/0, create/1, cons/2, is_empty/1, head/1, tail/1, foreach/2,
         map/2, nth/2, update/3]).
-include("binary_forest.hrl").
-include_lib("eunit/include/eunit.hrl").



create() -> create([]).
create(List) ->
    Size = length(List),
    #forest{size=Size, trees=trees(Size, List, [])}.


cons(Element, #forest{size=Size, trees=Trees}) ->
    Tree = #tree{size=1, value=Element},
    #forest{size=Size + 1, trees=merge(Tree, Trees)}.


is_empty(Forest) -> Forest#forest.size == 0.


head(#forest{trees=[Tree | _Trees]}) -> head_tree(Tree).


tail(Forest = #forest{trees=[]}) -> Forest;
tail(#forest{size=Size, trees=[Tree | Trees]}) ->
    #forest{size=Size - 1, trees=tail_tree(Tree, Trees)}.


foreach(Function, #forest{trees=Trees}) ->
    [ foreach_tree(Function, Tree) || Tree <- Trees ],
    ok.


map(Function, Forest = #forest{trees=Trees}) ->
    Forest#forest{trees=[ map_tree(Function, Tree) || Tree <- Trees ]}.


nth(N, Forest) -> nth_trees(N, Forest#forest.trees).


update(N, Value, Forest) ->
    Forest#forest{trees=update_trees(N, Value, Forest#forest.trees)}.



%%% helpers

highest_power_of_2(N) -> trunc(math:pow(2, trunc(math:log2(N)))).


%% Split a List of length Size into trees each of size 2^k.
trees(_Length, [], Trees) -> Trees;
trees(Length, List, Trees) ->
    TreeSize = highest_power_of_2(Length),
    {Rest, Values} = lists:split(Length - TreeSize, List), % take values off end
    trees(Length - TreeSize, Rest, [tree(TreeSize, Values) | Trees]).

tree(1, [Value]) -> #tree{size=1, value=Value};
tree(Size, Values) ->
    HalfSize = Size div 2,
    {LValues, RValues} = lists:split(HalfSize, Values),
    {LTree, RTree} = {tree(HalfSize, LValues), tree(HalfSize, RValues)},
    #tree{size=Size, left=LTree, right=RTree}.


%% Merge a Tree with other Trees of size 2^k (like adding 1 to a binary number)
merge(Tree, []) -> [Tree];
merge(Tree, [T | Trees]) when Tree#tree.size < T#tree.size -> [Tree, T | Trees];
merge(Tree, [T | Trees]) ->
    merge(#tree{size=2 * Tree#tree.size, left=Tree, right=T}, Trees).


head_tree(#tree{size=1, value=Value}) -> Value;
head_tree(#tree{left=L}) -> head_tree(L).


tail_tree(#tree{size=1}, Trees) -> Trees;
tail_tree(#tree{left=L, right=R}, Trees) -> tail_tree(L, [R | Trees]).


foreach_tree(Function, #tree{size=1, value=Value}) -> Function(Value);
foreach_tree(Function, #tree{left=L, right=R}) ->
    foreach_tree(Function, L), foreach_tree(Function, R).


map_tree(Function, Tree = #tree{size=1, value=Value}) ->
    Tree#tree{value=Function(Value)};
map_tree(Function, Tree = #tree{left=L, right=R}) ->
    Tree#tree{left=map_tree(Function, L), right=map_tree(Function, R)}.


nth_trees(N, [Tree = #tree{size=Size} | _Trees]) when N =< Size ->
    nth_tree(N, Tree);
nth_trees(N, [#tree{size=Size} | Trees]) -> nth_trees(N - Size, Trees).

nth_tree(1, #tree{size=1, value=Value}) -> Value;
nth_tree(N, #tree{size=Size, left=L}) when N =< Size div 2 -> nth_tree(N, L);
nth_tree(N, #tree{size=Size, right=R}) -> nth_tree(N - (Size div 2), R).


update_trees(N, Value, Trees) -> update_trees(N, Value, Trees, []).
update_trees(N, Value, [Tree = #tree{size=Size} | Trees], Out) when N > Size ->
    update_trees(N - Size, Value, Trees, [Tree | Out]);
update_trees(N, Value, [Tree | Trees], Out) ->
    lists:reverse([update_tree(N, Value, Tree) | Out]) ++ Trees.

update_tree(1, Value, Tree = #tree{size=1}) -> Tree#tree{value=Value};
update_tree(N, Value, Tree = #tree{size=Size, left=L}) when N =< Size div 2 ->
    Tree#tree{left=update_tree(N, Value, L)};
update_tree(N, Value, Tree = #tree{size=Size, right=R}) ->
    Tree#tree{right=update_tree(N - (Size div 2), Value, R)}.



%%% tests

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


highest_power_of_2_test() ->
    16 = highest_power_of_2(17),
    16 = highest_power_of_2(16),
    8 = highest_power_of_2(15),
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

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



%% tests (non-exports)

highest_power_of_2_test() ->
    16 = highest_power_of_2(17),
    16 = highest_power_of_2(16),
    8 = highest_power_of_2(15),
    ok.

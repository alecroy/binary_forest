-module(binary_forest).
-export([create/0, create/1, cons/2, is_empty/1, head/1, tail/1
 % foreach/2,
         % map/2, nth/2, update/3
         ]).
-include("binary_forest.hrl").
-include_lib("eunit/include/eunit.hrl").



create() -> create([]).
create(List) ->
    Size = length(List),
    #forest{size=Size, trees=trees(Size, List, [])}.


cons(Element, #forest{size=Size, trees=Trees}) ->
    Tree = #tree{size=1, value=Element, left=null, right=null},
    #forest{size=Size + 1, trees=merge(Tree, Trees)}.


is_empty(Forest) -> Forest#forest.size == 0.


head(#forest{trees=[Tree | _Trees]}) -> head_tree(Tree).


tail(Forest = #forest{trees=[]}) -> Forest;
tail(#forest{size=Size, trees=[Tree | Trees]}) ->
    #forest{size=Size - 1, trees=tail_tree(Tree) ++ Trees}.


% foreach(Function, #forest{trees=Trees}) ->
%     lists:foreach(fun (Tree) -> foreach_tree(Function, Tree) end, Trees).


% map(Function, Forest = #forest{trees=Trees}) ->
%     NewTrees = lists:map(fun (Tree) -> map_tree(Function, Tree) end, Trees),
%     Forest#forest{trees=NewTrees}.


% nth(N, Forest) -> nth_trees(N, Forest#forest.trees).


% update(N, Value, Forest) ->
%     Forest#forest{trees=update_trees(N, Value, Forest#forest.trees)}.



%%% helpers

highest_power_of_2(N) -> trunc(math:pow(2, trunc(math:log2(N)))).


%% Split a List of length Size into trees each of size 2^k.
trees(_Length, [], Trees) -> Trees;
trees(Length, List, Trees) ->
    TreeSize = highest_power_of_2(Length),
    {Rest, Values} = lists:split(Length - TreeSize, List), % take values off end
    trees(Length - TreeSize, Rest, [tree(TreeSize, Values) | Trees]).

tree(1, [Value]) -> #tree{size=1, value=Value, left=null, right=null};
tree(Size, Values) ->
    HalfSize = Size div 2,
    {LValues, RValues} = lists:split(HalfSize, Values),
    {LTree, RTree} = {tree(HalfSize, LValues), tree(HalfSize, RValues)},
    #tree{size=Size, value=null, left=LTree, right=RTree}.



%% Merge a Tree with other Trees of size 2^k (like adding 1 to a binary number)
merge(Tree, []) -> [Tree];
merge(Tree, [T | Trees]) when Tree#tree.size < T#tree.size ->
    [Tree] ++ [T | Trees];
merge(Tree, [T | Trees]) ->
    merge(#tree{size=2*Tree#tree.size, value=null, left=Tree, right=T}, Trees).


head_tree(#tree{size=1, value=Value}) -> Value;
head_tree(#tree{left=Left}) -> head_tree(Left).


tail_tree(#tree{size=1}) -> [];
tail_tree(Tree) -> tail_tree(Tree, []).

tail_tree(#tree{left=null}, Trees) -> Trees;
tail_tree(#tree{left=Left, right=Right}, Trees) ->
    tail_tree(Left, [Right | Trees]).

% foreach_tree(Function, #tree{values=Values}) ->
%     lists:foreach(Function, Values).


% map_tree(Function, Tree = #tree{values=Values}) ->
%     NewValues = lists:map(Function, Values),
%     Tree#tree{values=NewValues}.


% nth_trees(N, [#tree{size=Size} | Trees]) when N > Size ->
%     nth_trees(N - Size, Trees);
% nth_trees(N, [#tree{values=Values} | _Trees]) -> lists:nth(N, Values).


% update_trees(N, Value, Trees) -> update_trees(N, Value, Trees, []).
% update_trees(N, Value, [Tree = #tree{size=Size} | Trees], Out) when N > Size ->
%     update_trees(N - Size, Value, Trees, [Tree | Out]);
% update_trees(N, Value, [Tree | Trees], Out) ->
%     lists:reverse([update_tree(N, Value, Tree) | Out]) ++ Trees.

% update_tree(N, Value, Tree = #tree{values=Values}) ->
%     {Before, [_Old | After]} = lists:split(N - 1, Values), % [1..N-1], [N..]
%     Tree#tree{values=Before ++ [Value] ++ After}.



%%% tests

create_test() ->
    #forest{size=0, trees=[]} = create(),
    #forest{size=1, trees=[#tree{size=1, value=1, left=null, right=null}]}
        = create([1]),
    OneTwo = #forest{size=2,
                     trees=[#tree{size=2, value=null,
                                  left=#tree{size=1, value=1,
                                             left=null, right=null},
                                  right=#tree{size=1, value=2,
                                              left=null, right=null}}]},
    OneTwo = create([1, 2]),
    [T12] = OneTwo#forest.trees,
    #forest{size=4, trees=[#tree{size=4, value=null,
                                 left=T12, right=T12}]} = create([1, 2, 1, 2]),
    ok.


highest_power_of_2_test() ->
    16 = highest_power_of_2(17),
    16 = highest_power_of_2(16),
    8 = highest_power_of_2(15),
    ok.


cons_test() ->
    One = #forest{size=1, trees=[#tree{size=1, value=1,
                                       left=null, right=null}]},
    One = cons(1, create()),
    TwoThree = #forest{size=2,
                       trees=[#tree{size=2, value=null,
                                    left=#tree{size=1, value=2,
                                               left=null, right=null},
                                    right=#tree{size=1, value=3,
                                                left=null, right=null}}]},
    {[T1], [T23]} = {One#forest.trees, TwoThree#forest.trees},
    #forest{size=3, trees=[T1, T23]} = cons(1, cons(2, create([3]))),
    ok.


is_empty_test() ->
    true = is_empty(create()),
    false = is_empty(create([1])),
    ok.


tail_test() ->
    #forest{size=0, trees=[]} = tail(create([1])),
    #forest{size=1, trees=[#tree{size=1, value=2, left=null, right=null}]}
        = tail(create([1, 2])),
    ok.


% head_test() ->
%     1 = head(create([1])),
%     3 = head(tail(tail(create([1, 2, 3])))),
%     ok.


% map_test() ->
%     Identity = fun (X) -> X end,
%     Forest = create(lists:seq(1, 5)),
%     Forest = map(Identity, Forest),
%     Squares = create([1, 4, 9, 16, 25]),
%     Squares = map(fun (N) -> N * N end, Forest),
%     ok.


% nth_test() ->
%     1 = nth(1, create([1])),
%     100 = nth(100, create(lists:seq(1, 100))),
%     ok.


% update_test() ->
%     Original = create([1, 2, 3]),
%     Modified = create([1, 7, 3]),
%     Modified = update(2, 7, Original),
%     ok.

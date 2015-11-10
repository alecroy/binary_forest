-module(binary_forest).
-export([create/0, create/1]).
-include("binary_forest.hrl").
-include_lib("eunit/include/eunit.hrl").


create() -> create([]).
create(List) ->
	Size = length(List),
	#forest{size=Size, trees=trees(Size, List, [])}.

trees(_Size, [], Trees) -> Trees;
trees(Size, List, Trees) ->
	TreeSize = highest_power_of_2(Size),
	{Rest, Values} = lists:split(Size - TreeSize, List), % take values off end
	Tree = #tree{size=TreeSize, values=Values},
	trees(Size - TreeSize, Rest, [Tree | Trees]).

highest_power_of_2(N) -> trunc(math:pow(2, trunc(math:log2(N)))).


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

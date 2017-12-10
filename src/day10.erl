-module(day10).
-export([reverse_sublist/3, part1/1, funky_hash/2, duplicate_list/2, part2/1]).

reverse_sublist(List, Start, Len) ->
    %% erlang:display({List, Start, Len}),
    case Start + Len of
	N when N =< length(List) ->
	    lists:sublist(List, Start) ++ lists:reverse(lists:sublist(List, Start+1, Len)) ++ lists:nthtail(N, List);
	N ->
	    RevSubList = lists:reverse(lists:nthtail(Start, List) ++ lists:sublist(List, N - length(List))),
	    Rem = lists:sublist(List, N - length(List) + 1, length(List) - Len),
	    lists:nthtail(length(List) - Start, RevSubList) ++ Rem  ++ lists:sublist(RevSubList, length(List) - Start)
    end.

list_modifier(ModifierLength, {List, CurrentPosition, SkipSize}) ->
    {reverse_sublist(List, CurrentPosition, ModifierLength), (CurrentPosition + ModifierLength + SkipSize) rem length(List), SkipSize + 1}.

duplicate_list(List, N) -> duplicate_list(List, N, []).
duplicate_list(_, 0, Acc) ->
    Acc;
duplicate_list(List, N, Acc) ->
    duplicate_list(List, N-1, Acc ++ List).

funky_hash(Input, BaseList) ->
    lists:foldl(fun list_modifier/2, {BaseList, 0, 0}, Input).

part1(Input) ->
    funky_hash(Input, lists:seq(0, 255)).

xor_over_list(List) ->
     lists:foldl(fun(Num, Acc) -> Num bxor Acc end, 0, List).

batchwise_xor(List) -> batchwise_xor(List, []).
batchwise_xor([], Acc) ->
    Acc;
batchwise_xor(List, Acc) ->
    batchwise_xor(lists:nthtail(16, List), [xor_over_list(lists:sublist(List, 16))|Acc]).

part2(Input) ->
    Seed = [17, 31, 73, 47, 23],
    {JumbledBaseList, _, _} = funky_hash(duplicate_list(Input ++ Seed, 64), lists:seq(0, 255)),
    KHValues = batchwise_xor(JumbledBaseList),
    HexRep = lists:map(fun(X) -> integer_to_list(X,16) end, KHValues),
    erlang:display(HexRep),
    string:join(lists:reverse(HexRep), "").

    
			        
	

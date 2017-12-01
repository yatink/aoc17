-module(day1).
-export([calc_sum/1]).


% aggregator(T, AccIn) -> AccOut
aggregator({X, X}, Acc) ->
    Acc + X;
aggregator({_, _}, Acc) ->
    Acc.

calc_sum(List) ->
    {L, _} = lists:split(length(List) - 1, List),
    Cycled = [lists:last(List) | L],
    lists:foldl(fun aggregator/2, 0, lists:zip(List, Cycled)).
    

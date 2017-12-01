-module(day1).
-export([calc_sum/2, part1/1, part2/1]).


% aggregator(T, AccIn) -> AccOut
aggregator({X, X}, Acc) ->
    Acc + X;
aggregator({_, _}, Acc) ->
    Acc.

calc_sum(List, CycleLength) ->
    {L, R} = lists:split(length(List) - CycleLength, List),
    Cycled = R ++ L,
    lists:foldl(fun aggregator/2, 0, lists:zip(List, Cycled)).

part1(List) -> calc_sum(List, 1).
part2(List) -> calc_sum(List, length(List) div 2).


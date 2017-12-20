-module(day15).
-export([gen_next_value/3, gen_and_judge/2, part1/3]).

gen_next_value(Previous, Factor, Constraint) ->
    Next = (Previous * Factor) rem 2147483647,
    case Next rem Constraint of
        0 ->
            Next;
        _ -> gen_next_value(Next, Factor, Constraint)
    end.

genA_next_value(Previous) -> gen_next_value(Previous, 16807, 4).

genB_next_value(Previous) -> gen_next_value(Previous, 48271, 8).


gen_and_judge(Count, {CurrentA, CurrentB, Matches}) when Count > 0->
    NextA = genA_next_value(CurrentA),
    NextB = genB_next_value(CurrentB),
    case (CurrentA band 65535) =:= (CurrentB band 65535) of
        true ->
            gen_and_judge(Count - 1, {NextA, NextB, Matches + 1});
        false ->
            gen_and_judge(Count - 1, {NextA, NextB, Matches})
    end;
gen_and_judge(_, {_, _, Matches}) -> Matches.


part1(InitA, InitB, Max) ->
    gen_and_judge(Max, {InitA, InitB, 0}).

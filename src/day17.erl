-module(day17).
-export([spinlock/5, find_elem/3, part1/2, part2/2]).

spinlock(Acc, Iters, CurrentPosition, StepSize, MaxSize) when Iters < MaxSize ->
    erlang:display(hd(Acc)),
    NewPosition = 1 + ((CurrentPosition + StepSize) rem length(Acc)),
    spinlock(lists:sublist(Acc, NewPosition) ++ [Iters|lists:nthtail(NewPosition, Acc)], Iters + 1, NewPosition, StepSize, MaxSize);
spinlock(Acc, _, _, _, _) ->
    Acc.

rotate(Q, 0) -> Q;
rotate(Q, RotationDegrees) ->
    rotate(queue:in(queue:out(Q), Q), RotationDegrees - 1).

q_based_spinlock(Q, RotationDegrees, StepSize, CurrentIter, MaxIters) when CurrentIter < MaxIters ->
    NewPosition = (RotationDegrees + StepSize) rem CurrentIter,
    

find_elem([Elem|_], Elem, Index) -> Index;
find_elem([_|T], Elem, Index) -> find_elem(T, Elem, Index + 1).
   

part1(StepSize, Iters) ->
    L = spinlock([0], 1, 0, StepSize, Iters),
    lists:nth(find_elem(L, 2017, 1) + 1, L).

part2(StepSize, Iters) ->
    L = spinlock([0], 1, 0, StepSize, Iters).

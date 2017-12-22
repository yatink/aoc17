-module(day17).
-export([spinlock/5, find_elem/3, part1/2, part2/2, rotate/2, q_based_spinlock/5]).

spinlock(Acc, Iters, CurrentPosition, StepSize, MaxSize) when Iters < MaxSize ->
    erlang:display(hd(Acc)),
    NewPosition = 1 + ((CurrentPosition + StepSize) rem length(Acc)),
    spinlock(lists:sublist(Acc, NewPosition) ++ [Iters|lists:nthtail(NewPosition, Acc)], Iters + 1, NewPosition, StepSize, MaxSize);
spinlock(Acc, _, _, _, _) ->
    Acc.

rotate(Q, 0) -> Q;
rotate(Q, RotationDegrees) ->
    {{value, Elem}, NewQ} = queue:out(Q),
    rotate(queue:in(Elem, NewQ), RotationDegrees - 1).

q_based_spinlock(Q, RotationDegrees, StepSize, CurrentIter, MaxIters) when CurrentIter < MaxIters ->
    NewPosition = StepSize rem CurrentIter,
    %% erlang:display({queue:to_list(Q), RotationDegrees, CurrentIter, NewPosition}),
    %% RotatedQ = rotate(Q, NewPosition),
    q_based_spinlock(queue:in(CurrentIter, rotate(Q, NewPosition)), RotationDegrees + NewPosition, StepSize, CurrentIter + 1, MaxIters);
q_based_spinlock(Q, RotationDegrees, _, _, _) ->
    {Q, RotationDegrees}.


find_elem([Elem|_], Elem, Index) -> Index;
find_elem([_|T], Elem, Index) -> find_elem(T, Elem, Index + 1).
   

part1(StepSize, MaxIters) ->
    L = spinlock([0], 1, 0, StepSize, MaxIters),
    lists:nth(find_elem(L, 2017, 1) + 1, L).

part2(StepSize, MaxIters) ->
    q_based_spinlock(queue:from_list([0]), 0, StepSize, 1, MaxIters).

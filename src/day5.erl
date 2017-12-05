-module(day5).
-export([instruction_walk/5, part1/1, part2/1]).

instruction_walk(_, _, undefined, NoOfSteps, _) -> NoOfSteps;
instruction_walk(Instructions, Index, Value, NoOfSteps, OffsetUpdater) ->
    NewIndex = Index + Value,
    UpdatedOffset = OffsetUpdater(Value),
    UpdatedInstructions = array:set(Index, UpdatedOffset, Instructions),
    instruction_walk(UpdatedInstructions, NewIndex, array:get(NewIndex, UpdatedInstructions), NoOfSteps + 1, OffsetUpdater).

naiveUpdater(Value) ->
    Value + 1.

fancyUpdater(Value) when Value < 3 -> Value + 1;
fancyUpdater(Value) -> Value -1.
    

part1(Instructions) ->
    instruction_walk(Instructions, 0, array:get(0, Instructions), 0, fun naiveUpdater/1).

part2(Instructions) ->
    instruction_walk(Instructions, 0, array:get(0, Instructions), 0, fun fancyUpdater/1).

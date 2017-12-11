-module(day11).
-export([path_accumulator/2, path_reducer/1, part1/1, part2/1]).

% Cancel out NE and SW
path_reducer(#{"ne" := NESteps, "sw" := SWSteps} = ReducedSteps) when NESteps >= SWSteps, SWSteps =/= 0->
    path_reducer(maps:put("sw", 0, maps:put("ne", NESteps - SWSteps, ReducedSteps)));
path_reducer(#{"sw" := SWSteps, "ne" := NESteps} = ReducedSteps) when SWSteps > NESteps, NESteps =/= 0 ->
    path_reducer(maps:put("ne", 0, maps:put("sw", SWSteps - NESteps, ReducedSteps)));

% Cancel out NW and SE
path_reducer(#{"nw" := NWSteps, "se" := SESteps} = ReducedSteps) when NWSteps >= SESteps, SESteps =/= 0->
    path_reducer(maps:put("se", 0, maps:put("nw", NWSteps - SESteps, ReducedSteps)));
path_reducer(#{"se" := SESteps, "nw" := NWSteps} = ReducedSteps) when SESteps > NWSteps, NWSteps =/= 0 ->
    path_reducer(maps:put("nw", 0, maps:put("se", SESteps - NWSteps, ReducedSteps)));

% Convert NE + NW -> N
path_reducer(#{"ne" := NESteps, "nw" := NWSteps} = ReducedSteps) when NESteps >= NWSteps, NWSteps =/= 0 ->
    path_reducer(maps:put("nw", 0, maps:put("ne", NESteps - NWSteps, maps:put("n", maps:get("n", ReducedSteps, 0) + NWSteps, ReducedSteps))));
path_reducer(#{"nw" := NWSteps, "ne" := NESteps} = ReducedSteps) when NWSteps > NESteps, NESteps =/= 0 ->
    path_reducer(maps:put("ne", 0, maps:put("nw", NWSteps - NESteps, maps:put("n", maps:get("n", ReducedSteps, 0) + NESteps, ReducedSteps))));

% Convert SE + SW -> S
path_reducer(#{"se" := SESteps, "sw" := SWSteps} = ReducedSteps) when SESteps >= SWSteps, SWSteps =/= 0 ->
    path_reducer(maps:put("sw", 0, maps:put("se", SESteps - SWSteps, maps:put("s", maps:get("s", ReducedSteps, 0) + SWSteps, ReducedSteps))));
path_reducer(#{"sw" := SWSteps, "se" := SESteps} = ReducedSteps) when SWSteps > SESteps, SESteps =/= 0 ->
    path_reducer(maps:put("se", 0, maps:put("sw", SWSteps - SESteps, maps:put("s", maps:get("s", ReducedSteps, 0) + SESteps, ReducedSteps))));

% Cancel out N and S
path_reducer(#{"n" := NSteps, "s" := SSteps} = ReducedSteps) when NSteps >= SSteps, SSteps =/= 0->
    path_reducer(maps:put("s", 0, maps:put("n", NSteps - SSteps, ReducedSteps)));
path_reducer(#{"s" := SSteps, "n" := NSteps} = ReducedSteps) when SSteps > NSteps, NSteps =/= 0 ->
    path_reducer(maps:put("n", 0, maps:put("s", SSteps - NSteps, ReducedSteps)));

% Convert SE + N -> NE
path_reducer(#{"se" := SESteps, "n" := NSteps} = ReducedSteps) when SESteps >= NSteps, NSteps =/= 0 ->
    path_reducer(maps:put("n", 0, maps:put("se", SESteps - NSteps, maps:put("ne", maps:get("ne", ReducedSteps, 0) + NSteps, ReducedSteps))));
path_reducer(#{"n" := NSteps, "se" := SESteps} = ReducedSteps) when NSteps > SESteps, SESteps =/= 0 ->
    path_reducer(maps:put("se", 0, maps:put("n", NSteps - SESteps, maps:put("ne", maps:get("ne", ReducedSteps, 0) + SESteps, ReducedSteps))));

% Convert SW + N -> NW
path_reducer(#{"sw" := SWSteps, "n" := NSteps} = ReducedSteps) when SWSteps >= NSteps, NSteps =/= 0 ->
    path_reducer(maps:put("n", 0, maps:put("sw", SWSteps - NSteps, maps:put("nw", maps:get("nw", ReducedSteps, 0) + NSteps, ReducedSteps))));
path_reducer(#{"n" := NSteps, "sw" := SWSteps} = ReducedSteps) when NSteps > SWSteps, SWSteps =/= 0 ->
    path_reducer(maps:put("sw", 0, maps:put("n", NSteps - SWSteps, maps:put("nw", maps:get("nw", ReducedSteps, 0) + SWSteps, ReducedSteps))));

% Convert NE + S -> SE
path_reducer(#{"ne" := NESteps, "s" := SSteps} = ReducedSteps) when NESteps >= SSteps, SSteps =/= 0 ->
    path_reducer(maps:put("s", 0, maps:put("ne", NESteps - SSteps, maps:put("se", maps:get("se", ReducedSteps, 0) + SSteps, ReducedSteps))));
path_reducer(#{"s" := SSteps, "ne" := NESteps} = ReducedSteps) when SSteps > NESteps, NESteps =/= 0 ->
    path_reducer(maps:put("ne", 0, maps:put("s", SSteps - NESteps, maps:put("se", maps:get("se", ReducedSteps, 0) + NESteps, ReducedSteps))));

% Convert NW + S -> SW
path_reducer(#{"nw" := NWSteps, "s" := SSteps} = ReducedSteps) when NWSteps >= SSteps, SSteps =/= 0 ->
    path_reducer(maps:put("s", 0, maps:put("nw", NWSteps - SSteps, maps:put("sw", maps:get("sw", ReducedSteps, 0) + SSteps, ReducedSteps))));
path_reducer(#{"s" := SSteps, "nw" := NWSteps} = ReducedSteps) when SSteps > NWSteps, NWSteps =/= 0 ->
    path_reducer(maps:put("nw", 0, maps:put("s", SSteps - NWSteps, maps:put("sw", maps:get("sw", ReducedSteps, 0) + NWSteps, ReducedSteps))));


path_reducer(ReducedSteps) ->
    ReducedSteps.

% Accumulate all the paths into a dictionary
path_accumulator(Step, AccumulatedSteps) ->
    maps:put(Step, maps:get(Step, AccumulatedSteps, 0) + 1, AccumulatedSteps).

part1(Input) ->
    AccumulatedSteps = lists:foldl(fun path_accumulator/2, maps:new(), Input),
    ReducedSteps = path_reducer(AccumulatedSteps),
    lists:sum(maps:values(ReducedSteps)).

part2(Input) ->
    {_, MaxDist} = lists:foldl(
                     fun(Step, {StepsTillNow, MaxDistance}) ->
                             case part1([Step|StepsTillNow]) of
                                 Dist when Dist > MaxDistance -> {[Step|StepsTillNow], Dist};
                                 _ -> {[Step|StepsTillNow], MaxDistance}
                             end
                     end,
                     {[], 0}, Input),
    MaxDist.
        


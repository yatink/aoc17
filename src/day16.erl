-module(day16).
-export([interpret_step/2, part1/2, part2/4, apply_mapping/3, part2_cycle_detection/2]).

interpret_step(Step, ProgSequence) ->
    case Step of
        [$s | StrIndex ] ->
            Index = list_to_integer(StrIndex),
            lists:nthtail(length(ProgSequence) - Index, ProgSequence) ++ lists:sublist(ProgSequence, length(ProgSequence) - Index);
        [$x | Xchg ] ->
            [Pos1, Pos2] = string:tokens(Xchg, "/"),
            ProgsArray = array:from_list(ProgSequence),
            array:to_list(
              lists:foldl(
                fun({A,B}, Progs) -> array:set(A, array:get(B, ProgsArray), Progs) end, 
                ProgsArray,
                [{list_to_integer(X), list_to_integer(Y)} || X <- [Pos1, Pos2], Y <- [Pos1, Pos2], X =/= Y]));
        [$p | Xchg] ->
            [Prog1, Prog2] = string:tokens(Xchg, "/"),
            [Part1, Part2] = string:split(ProgSequence, Prog1),
            case string:split(Part1, Prog2) of
                [X] ->
                    [Part21, Part22] = string:split(Part2, Prog2),
                    X ++ Prog2 ++ Part21 ++ Prog1 ++ Part22;
                [X,Y] ->
                    X ++ Prog1 ++ Y ++ Prog2 ++ Part2
            end
    end.
                       
part1(Steps, InitialSequence) ->
    lists:foldl(fun interpret_step/2, InitialSequence, Steps).

apply_mapping(Mapping, Previous, Iters) when Iters > 0 ->
    erlang:display({Mapping, Previous, Iters}),
    apply_mapping(Mapping, maps:map(fun(_, V) -> maps:get(V, Mapping) end, Previous), Iters - 1);
apply_mapping(_, Previous, _) -> 
    erlang:display(Previous),
    Previous.
                    
gen_mapping_from_sequence(Sequence) ->
    lists:foldl(fun(Char, {Map, Index}) -> {maps:put(Index, Char - $a, Map), Index + 1} end, {maps:new(), 0}, Sequence).

gen_sequence_from_mapping(Mapping) ->
    lists:map(fun(X) -> $a + maps:get(X, Mapping) end, lists:seq(0, maps:size(Mapping) - 1)).
                      

part2(Steps, InitialSequence, X, Y) ->
    Sequence = part1(Steps, InitialSequence),
    {Initial, _} = gen_mapping_from_sequence(InitialSequence),
    {Mapping, _} = gen_mapping_from_sequence(Sequence),
    erlang:display("Running iterations"),
    GeneratedIters =  [X || _ <- lists:seq(1,Y)],
    erlang:display(GeneratedIters),
    RepeatedMapping = lists:foldl(fun(Iters, M) -> Z = apply_mapping(M, Initial, Iters), erlang:display({M, Iters, Initial}), Z end, Mapping, GeneratedIters),
    erlang:display(RepeatedMapping),
    gen_sequence_from_mapping(RepeatedMapping).
                        
part2_cycle_detection(Steps, InitialSequence) ->
    lists:foldl(
      fun(Iter, {Sequence, Sequences, Inv}) -> 
              NewSeq = part1(Steps, Sequence),
              erlang:display({Iter, maps:get(NewSeq, Sequences, [])}),
              {NewSeq, maps:put(NewSeq, [Iter|maps:get(NewSeq, Sequences, [])], Sequences), maps:put(Iter, NewSeq, Inv)}
      end, 
      {InitialSequence, maps:new(), maps:new()}, lists:seq(1, 100)).
    


-module(day14).
-export([number_of_ones/1, part1/1, generate_defrag_pattern/1, knot_hashify_decimal/2, count_regions/6, part2/1, flood_fill_region/5, pad_list/4]).

number_of_ones(Input) ->
    number_of_ones(Input, 0).
number_of_ones(0, Acc) ->
    Acc;
number_of_ones(Input, Acc)->
    number_of_ones(Input band (Input - 1), Acc + 1).

knot_hashify_decimal(Input, Row) ->
    InputString = lists:flatten(io_lib:format("~s-~w", [Input, Row])),
    HexRep = day10:part2(InputString),
    list_to_integer(HexRep, 16).
    
pad_list(List, PadElem, Length, TargetLength) when Length < TargetLength -> pad_list([PadElem | List], PadElem, Length + 1, TargetLength);
pad_list(List, _, _, _) -> List.

part1(Input) ->
    lists:foldl(
      fun(Row, Acc) -> Acc + number_of_ones(knot_hashify_decimal(Input, Row)) end,
      0, lists:seq(0, 127)).
    
			        
generate_defrag_pattern(Input) ->
    lists:foldl(
      fun(Row, Matrix) ->
              BinaryRep = integer_to_list(knot_hashify_decimal(Input, Row), 2),
              PaddedList = pad_list(BinaryRep, $0, length(BinaryRep), 128),
              %% erlang:display({PaddedList, length(PaddedList)}),
              array:set(Row, array:from_list(PaddedList), Matrix)
      end,
      array:new(),
      lists:seq(0, 127)).
		   

count_regions(DFPattern, XCoord, YCoord, RegionCount, PositiveValue, NegativeValue) when XCoord > 127->
    count_regions(DFPattern, 0, YCoord + 1, RegionCount, PositiveValue, NegativeValue);
count_regions(_, _, YCoord, RegionCount, _, _) when YCoord > 127 ->
    RegionCount;
count_regions(DFPattern, XCoord, YCoord, RegionCount, PositiveValue, NegativeValue) ->
    case array2D:get(XCoord, YCoord, DFPattern) of
	PositiveValue -> 
            %% erlang:display({XCoord, YCoord, RegionCount}),
            UpdatedPattern = flood_fill_region(DFPattern, XCoord, YCoord, PositiveValue, NegativeValue),
            count_regions(UpdatedPattern, XCoord + 1, YCoord, RegionCount + 1, PositiveValue, NegativeValue);
	_ -> count_regions(DFPattern, XCoord + 1, YCoord, RegionCount, PositiveValue, NegativeValue)
    end.
	     

flood_fill_region(DFPattern, XCoord, YCoord, PositiveValue, NegativeValue) ->
    case array2D:get(XCoord, YCoord, DFPattern) of
        PositiveValue ->
            %% erlang:display({XCoord, YCoord, PositiveValue}),
            Deltas = [0, 1, -1],
            Coords = [{XCoord + XDel, YCoord + YDel} || XDel <- Deltas, YDel <- Deltas, XCoord + XDel >= 0, YCoord + YDel >= 0, abs(XDel + YDel) =:= 1],
            ResetNeighbours = lists:foldl(
                                fun({X,Y}, M) -> flood_fill_region(M, X, Y, PositiveValue, NegativeValue) end,
                                array2D:set(XCoord, YCoord, DFPattern, NegativeValue), 
                                Coords),
            ResetNeighbours;
        _ ->
            %% erlang:display({XCoord, YCoord, done}),
            DFPattern
    end.

part2(Input) ->
    DFPattern = generate_defrag_pattern(Input),
    count_regions(DFPattern, 0, 0, 0, $1, $0).

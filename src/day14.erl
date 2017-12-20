-module(day14).
-export([number_of_ones/1, part1/1, generate_defrag_pattern/1, knot_hashify_decimal/2, count_regions/4, discover_region/3]).

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
    

part1(Input) ->
    lists:foldl(
      fun(Row, Acc) -> Acc + number_of_ones(knot_hashify_decimal(Input, Row)) end,
      0, lists:seq(0, 127)).
    
			        
generate_defrag_pattern(Input) ->
    {Pattern, _} = lists:foldl(
		     fun(Row, {Matrix, Count}) ->
			     {array:set(
				Count, 
				array:from_list(integer_to_list(knot_hashify_decimal(Input, Row), 2)),
				Matrix), Count + 1}
		     end, {array:new(), 0}, lists:seq(0, 127)),
    Pattern.
		   

count_regions(DFPattern, XCoord, YCoord, RegionCount) when XCoord > 127->
    count_regions(DFPattern, 0, YCoord + 1, RegionCount);
count_regions(_, _, YCoord, RegionCount) when YCoord > 127 ->
    RegionCount;
count_regions(DFPattern, XCoord, YCoord, RegionCount) ->
    case array2D:get(XCoord, YCoord, DFPattern) of
	$0 -> count_regions(DFPattern, XCoord + 1, YCoord, RegionCount);
	$1 -> count_regions(discover_region(DFPattern, XCoord, YCoord), XCoord + 1, YCoord, RegionCount + 1);
	_ -> count_regions(DFPattern, XCoord + 1, YCoord, RegionCount)
    end.
	     

discover_region(DFPattern, XCoord, YCoord) ->
    Deltas = [0, +1, -1],
    Region = case array2D:get(XCoord, YCoord, DFPattern) of		 
	$1 -> [{XCoord, YCoord} | [discover_region(DFPattern, XCoord + XDel, YCoord + YDel) || 
				      XDel <- Deltas, 
				      YDel <- Deltas, 
				      abs(XDel + YDel) =:= 1 ]
	      ];
	_ -> none
    end,
    lists:foldl(
      fun({X, Y}, Matrix) -> array2D:set(X, Y, $0, Matrix) end,
      DFPattern, 
      lists:filter(fun(X) -> X=/= none end, Region)).

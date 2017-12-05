-module(day3).
-export([find_ring_and_offset/1, find_side/2, calc_steps/3, part1/1, spiral_iterator/4, sum_at_point/2, aggregator/6, part2/1]).

%Given a spiral of numbers like so
%17  16  15  14  13
%18   5   4   3  12
%19   6   1   2  11
%20   7   8   9  10
%21   22  23  24 25
%determine which "ring" a given number 
%is in
%find_ring_and_offset(1) -> 0;
find_ring_and_offset(Query) ->
    find_ring_and_offset(Query, 1, 1, 1).

find_ring_and_offset(Query, RingMax, PrevRingMax, RingIndex) when Query =< RingMax ->
    {RingIndex, Query - PrevRingMax};
find_ring_and_offset(Query, RingMax, _, RingIndex) when Query > RingMax->
    find_ring_and_offset(Query, 8 div 2 * (RingIndex) * (RingIndex + 1) + 1, RingMax, RingIndex + 1).


%determine the side that the number falls into
find_side(RingIndex, Offset) when Offset =< 2 * (RingIndex - 1)-> right;
find_side(RingIndex, Offset) when Offset =< 4 * (RingIndex - 1) -> top;
find_side(RingIndex, Offset) when Offset =< 6 * (RingIndex - 1)-> left;
find_side(RingIndex, Offset) when Offset =< 8 * (RingIndex - 1)-> bottom.
    
%Find number of steps back to home
calc_steps(Offset,  RingIndex, right) -> abs(Offset - (RingIndex - 1)) + RingIndex - 1;
calc_steps(Offset, RingIndex, top) -> abs(Offset - 3 * (RingIndex - 1)) + RingIndex - 1; 
calc_steps(Offset, RingIndex, left) -> abs(Offset - 5 * (RingIndex - 1)) + RingIndex - 1; 
calc_steps(Offset, RingIndex, bottom) -> abs(Offset - 7 * (RingIndex - 1)) + RingIndex - 1.

part1(Query) ->
    {RingIndex, Offset} = find_ring_and_offset(Query),
    Side = find_side(RingIndex, Offset),
    calc_steps(Offset, RingIndex, Side).


rotate90(up) -> left;
rotate90(left) -> down;
rotate90(down) -> right;
rotate90(right) -> up.

move({X,Y}, left) -> {X-1, Y};
move({X,Y}, right) -> {X+1, Y};
move({X,Y}, up) -> {X, Y+1};
move({X,Y}, down) -> {X, Y-1}.
     

spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset =:= 0, Y-Y_offset =:= 0 -> {move({X,Y}, Direction), Direction};
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset =:= Y-Y_offset, X-X_offset >= 0 -> {move({X,Y}, rotate90(Direction)), rotate90(Direction)};
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset =:= Y-Y_offset, X-X_offset < 0 -> {move({X,Y}, rotate90(Direction)), rotate90(Direction)};
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X - X_offset < 0 , -1 * (X - X_offset) =:= Y-Y_offset -> {move({X,Y}, rotate90(Direction)), rotate90(Direction)};
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset >= 1, -1 * (X-X_offset) =:= Y-Y_offset - 1-> {move({X,Y}, rotate90(Direction)), rotate90(Direction)};
spiral_iterator({X,Y}, _, _, Direction) -> {move({X,Y}, Direction), Direction}.
    

sum_at_point(Matrix, {X,Y}) ->
    Deltas = [-1, 0, 1],
    Coords = [{X+Dx, Y+Dy} || Dx <- Deltas, Dy <- Deltas,  Dx =/= 0 orelse Dy =/= 0 ],
    Neighbours = lists:map(fun({_X,_Y}) -> array2D:get(_X, _Y, Matrix) end, Coords),
    lists:sum(lists:filter(fun(Val) -> Val =/= undefined end, Neighbours)).
				       

aggregator(Threshold, Matrix, Point, Direction, X_offset, Y_offset) ->
    Sum = sum_at_point(Matrix, Point),
    case Sum of
	    Sum when Sum > Threshold ->
			Sum;
	    _  ->
			{{X,Y}, Dir} = spiral_iterator(Point, X_offset, Y_offset, Direction),
			M = array2D:set(X,Y, Matrix, Sum),
			aggregator(Threshold, M, {X,Y}, Dir, X_offset, Y_offset)
	end.
	
part2(Threshold) ->
    Blank = array2D:new(200, 200),
    aggregator(Threshold, array2D:set(100, 100, Blank, 1), {1,0}, right, 100, 100). 

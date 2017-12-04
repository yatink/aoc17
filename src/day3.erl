-module(day3).
-export([find_ring_and_offset/1, find_side/2, calc_steps/3, part1/1, spiral_iterator/4]).

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
     

spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset =:= Y-Y_offset, X-X_offset > 0 -> move({X,Y}, left);
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X - X_offset < 0 , -1 * (X - X_offset) =:= Y-Y_offset -> move({X,Y}, down);
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset =:= Y-Y_offset, X-X_offset < 0 -> move({X,Y}, right);
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) when X-X_offset > 1, -1 * (X-X_offset) =:= Y-Y_offset - 1-> move({X,Y}, up);
spiral_iterator({X,Y}, X_offset, Y_offset, Direction) -> move({X,Y}, Direction).
    
    

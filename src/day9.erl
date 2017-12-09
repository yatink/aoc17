-module(day9).
-export([group_aggregator/5, part1/1, part2/1]).
%% GroupAggregator(Stream, GroupDepth, GroupCount, Mode)
%% Stream - Stream to process
%% CurrentGroupIndex - Current group depth
%% CurrentGroupCount - Count of all groups
%% GarbageCount - Number of non-cancelled garbage chars
%% Mode - tag the declares the current mode in the stream (garbage or regular)

group_aggregator([], _, GroupCount, GarbageCount, _) -> % End of the stream :)
    {GroupCount, GarbageCount}; 
group_aggregator([$<|Rest], GroupIndex, GroupCount, GarbageCount, regular) -> % Start a garbage section when you see a "<"
    group_aggregator(Rest, GroupIndex, GroupCount, GarbageCount, garbage);
group_aggregator([$!, _|Rest], GroupIndex, GroupCount, GarbageCount, garbage) -> % Inside a garbage section ignore the character afer a "!"
    group_aggregator(Rest, GroupIndex, GroupCount, GarbageCount, garbage);
group_aggregator([$>|Rest], GroupIndex, GroupCount, GarbageCount, garbage) -> % End a garbage section when you see a ">"
    group_aggregator(Rest, GroupIndex, GroupCount, GarbageCount, regular);
group_aggregator([_|Rest], GroupIndex, GroupCount, GarbageCount, garbage) -> % Increment Garbage count
    group_aggregator(Rest, GroupIndex, GroupCount, GarbageCount + 1, garbage); 
group_aggregator([${|Rest], GroupIndex, GroupCount, GarbageCount, regular) -> % Increment Group count and index when you see a "{"
    group_aggregator(Rest, GroupIndex + 1, GroupCount + GroupIndex + 1, GarbageCount, regular); 
group_aggregator([$}|Rest], GroupIndex, GroupCount, GarbageCount, regular) -> % Decrement Group index when you see a "}"
    group_aggregator(Rest, GroupIndex - 1, GroupCount, GarbageCount, regular);
group_aggregator([_|Rest], GroupIndex, GroupCount, GarbageCount, Mode) -> % Just keep chomping down other characters
    group_aggregator(Rest, GroupIndex, GroupCount, GarbageCount, Mode).
    
part1(Stream) ->
    element(1, group_aggregator(Stream, 0, 0, 0, regular)).

part2(Stream) ->
    element(2, group_aggregator(Stream, 0, 0, 0, regular)).

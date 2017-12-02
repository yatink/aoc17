-module(day2).
-export([part1/1, part2/1]).

max_deviation(Ints) ->
    % First Convert to a list of integers
    lists:max(Ints) - lists:min(Ints).

perfect_divisor(Ints) ->
    % First Convert to a list of integers
    hd([X div Y || X <- Ints, Y <- Ints, X rem Y =:= 0, X =/= Y]).

process_lines(Spreadsheet, Fn) ->
    Lines = string:tokens(Spreadsheet, "\n"),
    IntLines = lists:map(fun util:string_to_list_of_ints/1, Lines),
    Aggs = lists:map(Fn, IntLines),
    lists:sum(Aggs).
    
part1(Spreadsheet) ->
    process_lines(Spreadsheet, fun max_deviation/1).

part2(Spreadsheet) ->
    process_lines(Spreadsheet, fun perfect_divisor/1).

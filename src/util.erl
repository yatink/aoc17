-module(util).
-export([string_to_digits/1, string_to_list_of_ints/1, max_with_index/1, find_in_list/2]).

converter(X) when X >= 48, X =< 57 -> X - 48.

% Convert the string representation of a number into a list
% of digits.
%"1234" -> [1,2,3,4]
string_to_digits(String) ->
    lists:map(fun converter/1, String).

% Convert the string representation of space separate list of numbers
% into a list of ints
%"1234 1234 345" -> [1234, 1234, 345]
string_to_list_of_ints(Line) ->
    StringInts = string:tokens(Line, " "),
    lists:map(fun(X) -> element(1, string:to_integer(X)) end, StringInts).
    

% Find the maximum value and the corresponding index (zero indexed) in a list
max_with_index(List) ->
    max_with_index(sentinel, 0, 0, List).
max_with_index(sentinel, 0, 0, [H|T]) ->
    max_with_index(H, 0, 1, T);
max_with_index(Max, MaxIdx, _, []) ->
    {Max, MaxIdx};
max_with_index(Max, _, CurrIndex, [H|T]) when Max < H ->
    max_with_index(H, CurrIndex, CurrIndex + 1, T);
max_with_index(Max, MaxIdx, CurrIndex, [_|T]) ->
    max_with_index(Max, MaxIdx, CurrIndex + 1, T).

% Find the element in the list and return the corresponding index
find_in_list(Elem, List) ->
    find_in_list(Elem, List, 0).
find_in_list(Elem, [Elem|_], Idx) -> Idx;
find_in_list(Elem, [_|T], Idx) -> find_in_list(Elem, T, Idx + 1);
find_in_list(_, [], _) -> undefined.
    


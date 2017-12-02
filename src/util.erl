-module(util).
-export([string_to_digits/1, string_to_list_of_ints/1]).

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
    


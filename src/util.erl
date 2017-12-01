-module(util).
-export([string_to_ints/1]).

converter(X) when X >= 48, X =< 57 -> X - 48.

string_to_ints(String) ->
    lists:map(fun converter/1, String).

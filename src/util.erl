-module(util).
-export([string_to_ints/1]).

converter(48) -> 0;
converter(49) -> 1;
converter(50) -> 2;
converter(51) -> 3;
converter(52) -> 4;
converter(53) -> 5;
converter(54) -> 6;
converter(55) -> 7;
converter(56) -> 8;
converter(57) -> 9.

string_to_ints(String) ->
    lists:map(fun converter/1, String).

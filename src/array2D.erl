-module(array2D).
-export([new/0, get/3, set/4]).

new() -> array:new().

get(X, Y, Matrix) -> 
    Row = array:get(Y, Matrix),
    case Row of
	undefined -> undefined;
	Row -> array:get(X, Row)
    end.

set(X,Y, Matrix, Value) ->
    Row = array:get(Y, Matrix),
    NewRow = case Row of
	undefined -> array:set(X, Value, array:new());
	Row -> array:set(X, Value, Row)
    end,
    array:set(Y, NewRow, Matrix).


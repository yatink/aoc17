-module(array2D).
-export([new/2, get/3, set/4]).

new(Rows, Cols) ->
    R = array:new(Rows),
    array:map(fun(_, _)->array:new(Cols) end, R).

get(X, Y, Matrix) -> 
    Row = array:get(Y, Matrix),
    array:get(X, Row).
                      

set(X,Y, Matrix, Value) ->
    Row = array:get(Y, Matrix),
    NewRow = array:set(X, Value, Row),
    array:set(Y, NewRow, Matrix).
    

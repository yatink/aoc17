-module(day8).
-export([parse_instruction/2, evaluate_conditional/4, modify_registers/4, part1/1, part2/1]).

parse_instruction(Instruction, Registers) ->
    InstrTokens = string:tokens(Instruction, " "),
    Modifier = lists:takewhile(fun(Tok) -> Tok =/= "if" end, InstrTokens),
    [_|Conditional] = lists:dropwhile(fun(Tok) -> Tok =/= "if" end, InstrTokens),
    NewState = case apply(fun evaluate_conditional/4, Conditional ++ [Registers]) of
                   true -> apply(fun modify_registers/4, Modifier ++ [Registers]);
                   false -> Registers
               end,
    case maps:size(NewState) of
        0 -> {NewState, undefined};
        _ -> {NewState, lists:max(maps:values(NewState))}
    end.
    
                     

evaluate_conditional(Register, ">", Value, Registers) -> maps:get(Register, Registers, 0) > element(1, string:to_integer(Value));
evaluate_conditional(Register, "<", Value, Registers) -> maps:get(Register, Registers, 0) < element(1, string:to_integer(Value));
evaluate_conditional(Register, ">=", Value, Registers) -> maps:get(Register, Registers, 0) >= element(1, string:to_integer(Value));
evaluate_conditional(Register, "<=", Value, Registers) -> maps:get(Register, Registers, 0) =< element(1, string:to_integer(Value));
evaluate_conditional(Register, "==", Value, Registers) -> maps:get(Register, Registers, 0) =:= element(1, string:to_integer(Value));
evaluate_conditional(Register, "!=", Value, Registers) -> maps:get(Register, Registers, 0) =/= element(1, string:to_integer(Value)).    
    
modify_registers(Register, "inc", Value, Registers) -> maps:put(Register, maps:get(Register, Registers, 0) + element(1, string:to_integer(Value)), Registers);
modify_registers(Register, "dec", Value, Registers) -> maps:put(Register, maps:get(Register, Registers, 0) - element(1, string:to_integer(Value)), Registers).   
                                      
part1(Instructions) ->
    Registers = lists:foldl(fun(Instruction, Registers) -> element(1, parse_instruction(Instruction, Registers)) end, maps:new(), Instructions),
    lists:max(maps:values(Registers)).

part2(Instructions) ->
    {_, Max} = lists:foldl(
                  fun(Instruction, {Registers, Max}) -> 
                          case parse_instruction(Instruction, Registers) of
                              {NewState, undefined} -> {NewState, Max};
                              {NewState, NewMax} when Max =:= undefined -> {NewState, NewMax};
                              {NewState, NewMax} when NewMax > Max -> {NewState, NewMax};
                              {NewState, _} -> {NewState, Max}
                          end
                  end, 
                  {maps:new(), undefined}, Instructions),
    Max.

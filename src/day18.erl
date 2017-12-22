-module(day18).
-export([get_integer_or_register/2, parse_instruction/3, parse_instruction_set/4, part1/1]).

get_integer_or_register(StrValue, Registers) ->
    try list_to_integer(StrValue) of
        V -> V
    catch
        error:badarg -> maps:get(StrValue, Registers)
    end.
             
parse_instruction([Instr|Args], Registers, InstrModule) ->
    FnArgs = case Args of
                 [Register, StrValue] -> 
                     Value = get_integer_or_register(StrValue, Registers),
                     [Register, Value, Registers];
                 [Register] -> [Register, Registers]
             end,
    apply(list_to_atom(InstrModule), list_to_atom(Instr), FnArgs).


parse_instruction_set(InstructionSet, InstrPtr, Registers, InstrModule) ->
    case parse_instruction(string:tokens(array:get(InstrPtr, InstructionSet), " "), Registers, InstrModule) of
        {Freq, done} -> Freq;
        {NewState, Offset} -> parse_instruction_set(InstructionSet, InstrPtr + Offset, NewState, InstrModule)
    end.
            
part1(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    parse_instruction_set(Instructions, 0, maps:new(), "day18part1").
    
        
                                       
supervisor() ->
    %% 

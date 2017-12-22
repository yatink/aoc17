-module(day18).
-compile(export_all).

get_integer_or_register(StrValue, Registers) ->
    try list_to_integer(StrValue) of
        V -> V
    catch
        error:badarg -> maps:get(StrValue, Registers)
    end.
             

%% Instructions
set(Register, Value, Registers) -> {maps:put(Register, Value, Registers), 1}.
add(Register, Value, Registers) -> {maps:put(Register, Value + maps:get(Register, Registers, 0), Registers), 1}.
mul(Register, Value, Registers) -> {maps:put(Register, Value * maps:get(Register, Registers, 0), Registers), 1}.
mod(Register, Value, Registers) -> {maps:put(Register, maps:get(Register, Registers, 0) rem Value, Registers), 1}.
snd(Register, Registers) -> {maps:put(sound, maps:get(Register, Registers), Registers), 1}.
rcv(Check, Registers) -> 
    case Registers of
        Registers = #{Check := 0} -> {Registers, 1};
        _ -> {maps:get(sound, Registers), done}
    end.
jgz(Check, Offset, Registers) ->
    case get_integer_or_register(Check, Registers) of
        0 -> {Registers, 1};
        _ -> {Registers, Offset}
    end.


parse_instruction([Instr|Args], Registers) ->
    FnArgs = case Args of
                 [Register, StrValue] -> 
                     Value = get_integer_or_register(StrValue, Registers),
                     [Register, Value, Registers];
                 [Register] -> [Register, Registers]
             end,
    apply(day18, list_to_atom(Instr), FnArgs).


parse_instruction_set(InstructionSet, InstrPtr, Registers) ->
    case parse_instruction(string:tokens(array:get(InstrPtr, InstructionSet), " "), Registers) of
        {Freq, done} -> Freq;
        {NewState, Offset} -> parse_instruction_set(InstructionSet, InstrPtr + Offset, NewState)
    end.
            
part1(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    parse_instruction_set(Instructions, 0, maps:new()).
    
        
                                       

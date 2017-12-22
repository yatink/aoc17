-module(day18).
-export([get_integer_or_register/2, parse_instruction/3, parse_instruction_set/4, part1/1, supervisor/3, child/3, part2/1]).

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
    erlang:display({[Instr|Args], Registers}),
    apply(list_to_atom(InstrModule), list_to_atom(Instr), FnArgs).

parse_instruction_set(InstructionSet, InstrPtr, Registers, InstrModule) ->
    erlang:display({InstructionSet, InstrPtr}),
    case parse_instruction(string:tokens(array:get(InstrPtr, InstructionSet), " "), Registers, InstrModule) of
	wait -> {InstrPtr + 1, Registers};
        {Freq, done} -> Freq;
        {NewState, Offset} -> 
	    parse_instruction_set(InstructionSet, InstrPtr + Offset, NewState, InstrModule)
    end.
            
part1(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    parse_instruction_set(Instructions, 0, maps:new(), "day18part1").
                                       
supervisor(ChildrenPids, BlockedCount, SendCount) when BlockedCount < 2->
    lists:foreach(fun(Pid) -> Pid ! {start, self()} end, maps:values(ChildrenPids)),
    receive 
        {snd_request, Value, From} ->
            To = case From of
                     0 -> 1;
                     1 -> 0
                 end,
            maps:get(To, ChildrenPids) ! {send, Value},
            supervisor(ChildrenPids, BlockedCount, maps:put(From, maps:get(From, SendCount, 0) + 1, SendCount));
        {rcv_request, Child} ->
            maps:get(Child, ChildrenPids) ! {check_blocked},
            supervisor(ChildrenPids, BlockedCount, SendCount);
        blocked ->
	    erlang:display("Blocked"),
            supervisor(ChildrenPids, BlockedCount + 1, SendCount);
        not_blocked ->
	    erlang:display("Unblocked"),
            supervisor(ChildrenPids, BlockedCount - 1, SendCount)
    end;
supervisor(_, _, SendCount) -> 
    erlang:display(SendCount),
    SendCount.

child(Registers, InstructionSet, InstrPtr) ->
    receive 
	{start, Pid} -> 
	    {NewPtr, NewState} = parse_instruction_set(InstructionSet, InstrPtr, maps:put(supervisor, Pid, Registers), "day18part2"),
	    maps:get(supervisor, Registers) ! blocked,
	    child(NewState, InstructionSet, NewPtr);
	{send, Value} ->
	    maps:get(supervisor, Registers) ! not_blocked,
	    [_, Register] = string:tokens(array:get(InstrPtr, InstructionSet), " "),
	    {NewPtr,NewState} = parse_instruction_set(InstructionSet, InstrPtr + 1, maps:put(Register, Value, Registers), "day18part2"),
	    maps:get(supervisor, Registers) ! blocked,
	    child(NewState, InstructionSet, NewPtr)
    end.

part2(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    Child0 = spawn(day18, child, [#{"p" => 0}, Instructions, 0]),
    Child1 = spawn(day18, child, [#{"p" => 1}, Instructions, 0]),
    SuperVisorPid = spawn(day18, supervisor, [#{0 => Child0, 1 => Child1}, 0, #{0 => 0, 1 => 0}]),
    lists:foreach(fun(Pid) -> Pid ! {update_supervisor, SuperVisorPid} end, [Child0, Child1]),
    lists:foreach(fun(Pid) -> Pid ! {start} end, [Child0, Child1]).

-module(day18).
-export([get_integer_or_register/2, parse_instruction/3, parse_instruction_set/6, part1/1, child/3, part2/1, run_part2/0, run_part2_simple/0]).

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
                 [StrValue] -> 
		     [StrValue, Registers] 
             end,
    apply(list_to_atom(InstrModule), list_to_atom(Instr), FnArgs).
		

parse_instruction_set(InstructionSet, InstructionSetSize, InstrPtr, Registers, InstrModule, SendCount) when InstrPtr < InstructionSetSize->
    case parse_instruction(string:tokens(array:get(InstrPtr, InstructionSet), " "), Registers, InstrModule) of
	deadlock -> {deadlock, SendCount};
        {Freq, done} -> {complete_part1, Freq};
        {NewState, Offset} -> 
	    erlang:display("Nothing here"),
	    parse_instruction_set(InstructionSet, InstructionSetSize, InstrPtr + Offset, NewState, InstrModule, SendCount);
	{NewState, Offset, SendCountOffset} -> 
	    parse_instruction_set(InstructionSet, InstructionSetSize, InstrPtr + Offset, NewState, InstrModule, SendCount + SendCountOffset)
    end;
parse_instruction_set(_, _, _, _, _, SendCount) -> {finished_instr, SendCount}.

            
part1(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    parse_instruction_set(Instructions, array:size(Instructions), 0, maps:new(), "day18part1", 0).
                                       

child(Registers, InstructionSet, InstrPtr) ->
    receive 
	{_, {start, PeerPid}} -> 
	    Ret = parse_instruction_set(InstructionSet, array:size(InstructionSet), InstrPtr, maps:put(peer, PeerPid, Registers), "day18part2", 0),
	    erlang:display({start, Ret, InstrPtr, Registers})
    end.

start_processing(Pid, PeerPid) ->
    Pid ! {self(), {start, PeerPid}}.
				        
create(Id, Instructions) ->
    spawn(day18, child, [#{"p" => Id, self => Id}, Instructions, 0]).

part2(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    %% erlang:display("Spawning Children"),
    Child0 = create(0, Instructions),
    Child1 = create(1, Instructions),

    start_processing(Child0, Child1),
    start_processing(Child1, Child0).

run_part2() ->
    Ip = string:tokens(
	   "set i 31
set a 1
mul p 17
jgz p p
mul a 2
add i -1
jgz i -2
add a -1
set i 127
set p 826
mul p 8505
mod p a
mul p 129749
add p 12345
mod p a
set b p
mod b 10000
snd b
add i -1
jgz i -9
jgz a 3
rcv b
jgz b -1
set f 0
set i 126
rcv a
rcv b
set p a
mul p -1
add p b
jgz p 4
snd a
set a b
jgz 1 3
snd b
set f 1
add i -1
jgz i -11
snd a
jgz f -16
jgz a -19", "\n"), 
    part2(Ip).

run_part2_simple() ->
    Ip = string:tokens(
	   "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d", "\n"),
    part2(Ip).

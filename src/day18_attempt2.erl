-module(day18_attempt2).
-export([get_integer_or_register/2, set/5, add/5, mul/5, mod/5, jgz/5, snd/4, rcv/4, execute_instruction/4, run_duet/9, part2/1, run_part2_simple/0, run_part2/0]).

get_integer_or_register(StrValue, Registers) ->
    try list_to_integer(StrValue) of
        V -> V
    catch
        error:badarg -> maps:get(StrValue, Registers)
    end.

set(Register, Value, Registers, QSelf, QPeer) -> {maps:put(Register, Value, Registers), QSelf, QPeer, 1, 0}.
add(Register, Value, Registers, QSelf, QPeer) -> {maps:put(Register, maps:get(Register, Registers) + Value, Registers), QSelf, QPeer, 1, 0}.
mul(Register, Value, Registers, QSelf, QPeer) -> {maps:put(Register, maps:get(Register, Registers) * Value, Registers), QSelf, QPeer, 1, 0}.
mod(Register, Value, Registers, QSelf, QPeer) -> {maps:put(Register, maps:get(Register, Registers) rem Value, Registers), QSelf, QPeer, 1, 0}.
jgz(Value1, Value2, Registers, QSelf, QPeer) ->
    case get_integer_or_register(Value1, Registers) of
	0 -> {Registers, QSelf, QPeer, 1, 0};
	_ -> {Registers, QSelf, QPeer, Value2, 0}
    end.
snd(Value1, Registers, QSelf, QPeer) ->
    {Registers, QSelf, queue:in(get_integer_or_register(Value1, Registers), QPeer), 1, 1}.
rcv(Register, Registers, QSelf, QPeer) ->
    case queue:out(QSelf) of
	{empty, Q} -> {Registers, Q, QPeer, 0, 0};
	{{value, V}, Q} -> {maps:put(Register, V, Registers), Q, QPeer, 1, 0}
    end.

execute_instruction([Instr|Args], Registers, QSelf, QPeer) ->
    case Args of
	[Arg1] -> 
	    case Instr of
		"snd" -> snd(Arg1, Registers, QSelf, QPeer);
		"rcv" -> rcv(Arg1, Registers, QSelf, QPeer)
	    end;
	[Arg1, Arg2] ->
	    Value2 = get_integer_or_register(Arg2, Registers),
	    case Instr of
		"set" -> set(Arg1, Value2, Registers, QSelf, QPeer);
		"add" -> add(Arg1, Value2, Registers, QSelf, QPeer);
		"mul" -> mul(Arg1, Value2, Registers, QSelf, QPeer);
		"mod" -> mod(Arg1, Value2, Registers, QSelf, QPeer);                                    
		"jgz" -> jgz(Arg1, Value2, Registers, QSelf, QPeer)
	    end
    end.

run_duet(RegistersSelf, RegistersPeer, Instructions, InstrPtrSelf, InstrPtrPeer, QSelf, QPeer, SendCountSelf, SendCountPeer) ->
    {Registers, QS, QP, IOffset, SOffset} = execute_instruction(
							      string:tokens(array:get(InstrPtrSelf, Instructions), " "),
							      RegistersSelf,
							      QSelf,
							      QPeer),
    case IOffset of
	0 ->
	    case {queue:is_empty(QP), InstrPtrPeer} of
		{true, 0} -> run_duet(RegistersPeer, Registers, Instructions, InstrPtrPeer, InstrPtrSelf, QP, QS, SendCountPeer, SendCountSelf);
		{true, _} -> {SendCountSelf, SendCountPeer};
		{false, _} -> run_duet(RegistersPeer, Registers, Instructions, InstrPtrPeer, InstrPtrSelf, QP, QS, SendCountPeer, SendCountSelf)
	    end;
	_ -> run_duet(Registers, RegistersPeer, Instructions, InstrPtrSelf + IOffset, InstrPtrPeer, QS, QP, SendCountSelf + SOffset, SendCountPeer)
    end.
				    
		    
part2(InstructionStrings) ->
    {Instructions, _} = lists:foldl(
                          fun(Instr, {InstrSet, Index}) -> {array:set(Index, Instr, InstrSet), Index + 1} end,
                          {array:new(), 0},
                          InstructionStrings),
    
    run_duet(#{"p" => 0}, #{"p" => 1}, Instructions, 0, 0, queue:new(), queue:new(), 0, 0).
		   
	
    
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

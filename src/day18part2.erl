-module(day18part2).
-compile(export_all).

%% Instructions
set(Register, Value, Registers) -> {maps:put(Register, Value, Registers), 1, 0}.
add(Register, Value, Registers) -> {maps:put(Register, Value + maps:get(Register, Registers), Registers), 1, 0}.
mul(Register, Value, Registers) -> {maps:put(Register, Value * maps:get(Register, Registers), Registers), 1, 0}.
mod(Register, Value, Registers) -> {maps:put(Register, maps:get(Register, Registers) rem Value, Registers), 1, 0}.
snd(StrValue, Registers) ->
    send_value(maps:get(peer, Registers), day18:get_integer_or_register(StrValue, Registers)),
    {Registers, 1, 1}.
rcv(Register, Registers) -> 
    receive
	{_, {send, Value}} ->
	    {maps:put(Register, Value, Registers), 1, 0}
    after 5000 ->
	    deadlock
    end.
jgz(Check, Offset, Registers) ->
    case day18:get_integer_or_register(Check, Registers) of
        0 -> {Registers, 1, 0};
        _ -> {Registers, Offset, 0}
    end.

send_value(Pid, Value) ->
    Pid ! {self(), {send, Value}}.

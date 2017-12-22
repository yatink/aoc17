-module(day18part2).
-compile(export_all).

%% Instructions
set(Register, Value, Registers, _) -> {maps:put(Register, Value, Registers), 1}.
add(Register, Value, Registers, _) -> {maps:put(Register, Value + maps:get(Register, Registers, 0), Registers), 1}.
mul(Register, Value, Registers, _) -> {maps:put(Register, Value * maps:get(Register, Registers, 0), Registers), 1}.
mod(Register, Value, Registers, _) -> {maps:put(Register, maps:get(Register, Registers, 0) rem Value, Registers), 1}.
snd(Value, Registers) -> 
    maps:get(supervisor, Registers) ! {snd_request, day18:get_integer_or_register(Value, Registers), maps:get("p", Registers)},
    {Registers, 1}.
rcv(Register, Registers) -> 
    maps:get(supervisor, Registers) ! {rcv_request, Register},
    {Register, wait}.
jgz(Check, Offset, Registers) ->
    case day18:get_integer_or_register(Check, Registers) of
        0 -> {Registers, 1};
        _ -> {Registers, Offset}
    end.

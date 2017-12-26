-module(day18part1).
-compile(export_all).

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
    case day18:get_integer_or_register(Check, Registers) of
        0 -> {Registers, 1};
        _ -> {Registers, Offset}
    end.

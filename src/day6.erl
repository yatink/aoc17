-module(day6).
-export([redistribute/4, mem_rearrange/2, part1/1, part2/1]).

redistribute(Banks, _, 0, _) ->
    array:to_list(Banks);
redistribute(Banks, CurrentIndex, ReDist, Size) when CurrentIndex < Size ->
    CurrentValue = array:get(CurrentIndex, Banks),
    redistribute(array:set(CurrentIndex, CurrentValue + 1, Banks), CurrentIndex + 1, ReDist - 1, Size);
redistribute(Banks, _, ReDist, Size) -> redistribute(Banks, 0, ReDist, Size).

mem_rearrange(PrevBanks, CurrBank) ->
    case lists:member(CurrBank, PrevBanks) of
        true -> {PrevBanks, CurrBank};
        _ -> {Max, MaxIdx} = util:max_with_index(CurrBank),
             ResetMaxBank = array:set(MaxIdx, 0, array:from_list(CurrBank)),
             NewBank = redistribute(ResetMaxBank, MaxIdx + 1, Max, length(CurrBank)),
             mem_rearrange([CurrBank|PrevBanks], NewBank)
    end.

part1(Bank) ->
    {PrevBanks, _} = mem_rearrange([], Bank),
    length(PrevBanks).


part2(Bank) ->
    {PrevBanks, CurrBank} = mem_rearrange([], Bank),
    erlang:display({PrevBanks, CurrBank}),
    util:find_in_list(CurrBank, PrevBanks).

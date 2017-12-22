-module(day13).
-export([parse_scanners/2, severity_aggregator/4, part1/1, part2/1, get_through_firewall/2, check_firewall/4]).

parse_scanners(Line, Scanners) ->
    [L|[R|[]]] = string:tokens(Line, ":"),
    maps:put(list_to_integer(L), list_to_integer(string:strip(R)), Scanners).

severity_aggregator(Scanners, CurrentTime, Severity, MaxScannerDepth) when CurrentTime =< MaxScannerDepth ->
    case Scanners of
	Scanners = #{CurrentTime:= Range} when CurrentTime rem (2 * (Range - 1)) =:= 0 ->     
	    severity_aggregator(Scanners, CurrentTime + 1, Severity + CurrentTime * Range, MaxScannerDepth);
	Scanners = #{CurrentTime:= Range} when CurrentTime rem (2 * (Range - 1)) =/= 0 ->     
	    severity_aggregator(Scanners, CurrentTime + 1, Severity, MaxScannerDepth);
	_ ->
	    severity_aggregator(Scanners, CurrentTime + 1, Severity, MaxScannerDepth)
    end;
severity_aggregator(_, _, Severity, _) -> Severity.


check_firewall(Scanners, CurrentTime, Delay, MaxScannerDepth) when CurrentTime - Delay =< MaxScannerDepth ->
    CurrentDepth = CurrentTime - Delay,
    case Scanners of
	Scanners = #{CurrentDepth:= Range} when CurrentTime rem (2 * (Range - 1)) =:= 0 ->
	    false;
	Scanners = #{CurrentDepth:= Range} when CurrentTime rem (2 * (Range - 1)) =/= 0 ->     
	    check_firewall(Scanners, CurrentTime + 1, Delay, MaxScannerDepth);
	_ ->
	    check_firewall(Scanners, CurrentTime + 1, Delay, MaxScannerDepth)
    end;
check_firewall(_, _, _, _) -> true.


part1(Input) ->	
    Scanners = lists:foldl(fun parse_scanners/2, maps:new(), Input),
    severity_aggregator(Scanners, 0, 0, lists:max(maps:keys(Scanners))).

get_through_firewall(Scanners, Delay) ->
    case check_firewall(Scanners, Delay, Delay, lists:max(maps:keys(Scanners))) of
	true  -> Delay;
	_ -> get_through_firewall(Scanners, Delay + 1)
    end.


part2(Input) ->
    Scanners = lists:foldl(fun parse_scanners/2, maps:new(), Input),
    get_through_firewall(Scanners, 0).

-module(day7).
-export([parse_input_to_sets/1, part1/1, part2/1, parse_link/1, parse_weight/1, weight/3, find_imbalance/3]).

parse_input_to_sets(Lines) ->
    Parents = sets:from_list(lists:map(fun(Line) ->
                                               Left = hd(string:tokens(Line, "->")),
                                               Name = hd(string:tokens(Left, " ")),
                                               Name end,
                                       Lines)),
    Children = sets:from_list(lists:flatmap(fun(Line) ->
                                                    case string:tokens(Line, "->") of
                                                        [_|[Right|[]]] -> 
                                                            lists:map(fun(Tok) -> string:strip(Tok) end, string:tokens(Right, ","));
                                                        _ -> []
                                                    end
                                            end, 
                                            Lines)),
    {Parents, Children}.
                              
        
part1(Lines) ->
    {Parents, Children} = parse_input_to_sets(Lines),
    hd(sets:to_list(sets:subtract(Parents, Children))).

parse_link(Line) ->
    case string:tokens(Line, "->") of
        [Left|[]] -> 
            [Name|_] = string:tokens(Left, " "),
            {Name, []};
        [Left|[Right|[]]] ->
            [Name|_] = string:tokens(Left, " "),
            {Name, lists:map(fun(Tok) -> string:trim(Tok) end, string:tokens(Right, ","))}
    end.

parse_weight(Line) ->
    [Left|_] = string:tokens(Line, "->"),
    [Name|[Wt|[]]] = string:tokens(Left, " "),
    {Name, element(1, string:to_integer(string:trim(Wt, both, "() ")))}.

part2(Lines) ->
    Links = maps:from_list(lists:map(fun parse_link/1, Lines)),
    Weights = maps:from_list(lists:map(fun parse_weight/1, Lines)),
    % Find the parent of the bad node
    ImbalancedParent = lists:last(find_imbalance(part1(Lines), Links, Weights)),
    % Find the bad child node
    GetChildrenWeights = lists:map(fun(Child) -> {Child, weight(Child, Links, Weights)} end, maps:get(ImbalancedParent, Links)),
    % Find child node with "different" weight
    GroupedByWeight = lists:foldl(
                        fun({Child, Wt}, AccIn) ->
                                maps:put(Wt, [Child|maps:get(Wt, AccIn, [])], AccIn) 
                        end,
                        maps:new(),
                        GetChildrenWeights),
    [BadChildren|[]] = lists:filter(fun(Children) -> length(Children) =:= 1 end, maps:values(GroupedByWeight)),
    lists:map(fun(Child) -> {Child, maps:get(Child, Weights), weight(Child, Links, Weights)} end, BadChildren).
                      
                                           

    

weight(Node, Links, Weights) ->
    maps:get(Node, Weights) + lists:sum(lists:map(fun(Child) -> weight(Child, Links, Weights) end, maps:get(Node, Links))).

find_imbalance(Node, Links, Weights) ->
    case maps:get(Node, Links) of
        [] ->
            [];
        Children ->
                case sets:size(sets:from_list(lists:map(fun(Ch) -> weight(Ch, Links, Weights) end, Children))) of %Check if the weights are the same or not..
                1 ->
                    [];
                _ ->
                    % imbalance found...add current node to list of imbalanced nodes
                    [Node|lists:flatmap(fun(Ch) -> find_imbalance(Ch, Links, Weights) end, Children)]
            end
    end.

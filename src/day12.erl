-module(day12).
-export([parse_input/1, add_dependency/3, construct_deps/1, part1/1, gather_dependencies/2, find_groups/3, part2/1]).

parse_input(Line) ->
    [L|[R|[]]] = string:tokens(Line, "<->"),
    {element(1, string:to_integer(L)), lists:map(fun(X) -> element(1, string:to_integer(X)) end, string:tokens(R, ", "))}.


add_dependency(Key, Values, Dependencies) ->
    maps:put(Key, sets:union(maps:get(Key, Dependencies, sets:new()), sets:from_list(Values)), Dependencies).

construct_deps(Lines) ->
    lists:foldl(
      fun({L,R}, Dependencies) ->
	      UpdateL = add_dependency(L, R, Dependencies),
	      lists:foldl(fun(Y, Deps) -> add_dependency(Y, [L], Deps) end, UpdateL, R)
      end,
      maps:new(),
      Lines).

gather_dependencies(Dependencies, Node) -> 
    case Dependencies of
	Dependencies = #{Node := Children} ->
	    lists:foldl(fun sets:union/2, Children, lists:map(fun(Ch) -> gather_dependencies(maps:remove(Node, Dependencies), Ch) end, sets:to_list(Children)));
	_ ->
	    sets:new()
    end.
						      
part1(Lines) ->
    Dependencies = construct_deps(lists:map(fun parse_input/1, Lines)),
    gather_dependencies(Dependencies, 0).


find_groups(Dependencies, [Prg|Unaccounted], NumGroups) ->
    Group = gather_dependencies(Dependencies, Prg),
    find_groups(Dependencies, sets:to_list(sets:subtract(sets:from_list([Prg|Unaccounted]), Group)), NumGroups + 1);
find_groups(_, [], NumGroups) ->
    NumGroups.
	

part2(Lines) ->
    Dependencies = construct_deps(lists:map(fun parse_input/1, Lines)),
    find_groups(Dependencies, maps:keys(Dependencies), 0).

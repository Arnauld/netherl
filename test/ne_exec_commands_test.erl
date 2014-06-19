-module(ne_exec_commands_test).
-include("netherl.hrl").
-include_lib("eunit/include/eunit.hrl").

move_forward_test() ->
	move_forward_parameterized([4, 7], north, [4, 6]),
	move_forward_parameterized([4, 7], south, [4, 8]),
	move_forward_parameterized([4, 7], west,  [3, 7]),
	move_forward_parameterized([4, 7], east,  [5, 7]).

move_forward_parameterized(Location, Direction, ExpectedLocation) ->
	World = #world{},
	% Prgm  = #program{},
	Exec  = #execution{location=Location, direction=Direction},
	{World1, Exec1} = ne_exec_commands:move_forward({World, Exec}),
	Loc = Exec1#execution.location,
	?assertEqual(ExpectedLocation, Loc),
	% World shoud remain unchanged
	?assertEqual(World, World1).


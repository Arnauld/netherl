-module(ne_commands_test).
-include("netherl.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%%
%%

move_forward_north_test() ->
	move_forward_parameterized([4, 7], north, [4, 6]).

move_forward_south_test() ->
	move_forward_parameterized([4, 7], south, [4, 8]).

move_forward_west_test() ->
	move_forward_parameterized([4, 7], west,  [3, 7]).

move_forward_east_test() ->
	move_forward_parameterized([4, 7], east,  [5, 7]).

move_forward_parameterized(Location, Direction, ExpectedLocation) ->
	% Given
	World = ne_world:new_world(),
	Prgm  = #program{},
	Exec  = #execution{location=Location, direction=Direction},
	%
	% When
	Events = ne_commands:execute(move_forward, {World, Exec, Prgm}),
	[{program_moved, Loc}] = Events,
	%
	% Then
	?assertEqual(ExpectedLocation, Loc).
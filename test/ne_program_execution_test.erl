-module(ne_program_execution_test).
-include("netherl.hrl").
-include("asserts.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ------------------
%% move_forward
%% ------------------

move_forward_in_north_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, north),
    Exec1 = ne_program_execution:move_forward(Exec0, World),
    ?assertEqual({1, 1}, ne_program_execution:location(Exec1)),
    Events = ne_program_execution:uncommited_events(Exec1),
    Types  = ne_events:extract_types(Events),
    ?assertEqual([program_moved], Types).
    
move_forward_in_west_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, west),
    Exec1 = ne_program_execution:move_forward(Exec0, World),
    ?assertEqual({0, 2}, ne_program_execution:location(Exec1)).
    
move_forward_in_souh_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, south),
    Exec1 = ne_program_execution:move_forward(Exec0, World),
    ?assertEqual({1, 3}, ne_program_execution:location(Exec1)).
    
move_forward_in_east_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, east),
    Exec1 = ne_program_execution:move_forward(Exec0, World),
    ?assertEqual({2, 2}, ne_program_execution:location(Exec1)).

move_forward_should_fail_when_moving_outside_of_the_world_test() ->
    {World, Exec0} = prepare_exec({2, 2}, east),
    try
        ne_program_execution:move_forward(Exec0, World),
        ?fail("an exception should have been raised")
    catch
        Error:Reason ->
            ?assertEqual({Error, Reason}, {throw, {illegal_move,{3,2}}})
    end.


    
%% ------------------
%% rotate_left
%% ------------------

rotate_left_from_east_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, east),
    Exec1 = ne_program_execution:rotate_left(Exec0),
    ?assertEqual(north, ne_program_execution:direction(Exec1)).

rotate_left_from_north_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, north),
    Exec1 = ne_program_execution:rotate_left(Exec0),
    ?assertEqual(west, ne_program_execution:direction(Exec1)).

rotate_left_from_west_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, west),
    Exec1 = ne_program_execution:rotate_left(Exec0),
    ?assertEqual(south, ne_program_execution:direction(Exec1)).

rotate_left_from_south_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, south),
    Exec1 = ne_program_execution:rotate_left(Exec0),
    ?assertEqual(east, ne_program_execution:direction(Exec1)).

%% ------------------
%% rotate_right
%% ------------------

rotate_right_from_east_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, east),
    Exec1 = ne_program_execution:rotate_right(Exec0),
    ?assertEqual(south, ne_program_execution:direction(Exec1)).

rotate_right_from_north_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, north),
    Exec1 = ne_program_execution:rotate_right(Exec0),
    ?assertEqual(east, ne_program_execution:direction(Exec1)).

rotate_right_from_west_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, west),
    Exec1 = ne_program_execution:rotate_right(Exec0),
    ?assertEqual(north, ne_program_execution:direction(Exec1)).

rotate_right_from_south_direction_test() ->
    {World, Exec0} = prepare_exec({1, 2}, south),
    Exec1 = ne_program_execution:rotate_right(Exec0),
    ?assertEqual(west, ne_program_execution:direction(Exec1)).


prepare_exec(Location, Direction) ->
    World = ne_world:new_world([
            {{0,2}, ne_block:new_block()},
            {{1,1}, ne_block:new_block()},
            {{1,2}, ne_block:new_block()},
            {{2,2}, ne_block:new_block()},
            {{1,3}, ne_block:new_block()},
            {{1,4}, ne_block:new_block()},
            {{1,5}, ne_block:new_block()},
            {{2,5}, ne_block:new_block()},
            {{3,5}, ne_block:new_block()}
        ]),
    Exec0 = ne_program_execution:new("d06f00d"),
    Exec1 = ne_program_execution:init_program(Exec0, #program{}),
    Exec2 = ne_program_execution:locate_at(Exec1, Location),
    Exec3 = ne_program_execution:look_at(Exec2, Direction),
    % consume uncommitted events...
    Exec4 = ne_program_execution:process_unsaved_changes(Exec3, fun(Id, Events) -> ignored end),
    {World, Exec4}.


%% ------------------
%% load_from_history
%% ------------------

load_from_history_test() ->
    World  = ne_world:new_world(),
    Events = [{program_initialized,        101, {program, []}},
              {program_location_adjusted,  102, {1,2}, init},
              {program_direction_adjusted, 103, south, init},
              {program_moved,              104, {1,3}},
              {program_moved,              105, {1,4}},
              {program_moved,              106, {1,5}},
              {program_left_rotated,       107, east},
              {program_moved,              108, {2,5}}],
    Exec0 = ne_program_execution:new("d06f00d"),
    Exec1 = ne_program_execution:load_from_history(Exec0, Events),
    ?assertEqual({2, 5}, ne_program_execution:location(Exec1)),
    ?assertEqual(east,   ne_program_execution:direction(Exec1)),
    ?assertEqual([],     ne_program_execution:uncommited_events(Exec1)).

%% ------------------
%% usecases
%% ------------------

usecase_001_test() ->
    World = ne_world:new_world([
            {{1,2}, ne_block:new_block()},
            {{1,3}, ne_block:new_block()},
            {{1,4}, ne_block:new_block()},
            {{1,5}, ne_block:new_block()},
            {{2,5}, ne_block:new_block()},
            {{3,5}, ne_block:new_block()}
        ]),
    Exec0 = ne_program_execution:new("d06f00d"),
    Exec1 = ne_program_execution:init_program(Exec0, #program{}),
    Exec2 = ne_program_execution:locate_at(Exec1, {1, 2}),
    Exec3 = ne_program_execution:look_at(Exec2, south),
    Exec4 = ne_program_execution:move_forward(Exec3, World),
    Exec5 = ne_program_execution:move_forward(Exec4, World),
    Exec6 = ne_program_execution:move_forward(Exec5, World),
    Exec7 = ne_program_execution:rotate_left(Exec6),
    Exec8 = ne_program_execution:move_forward(Exec7, World),
    ?assertEqual({2, 5}, ne_program_execution:location(Exec8)),
    Events = ne_program_execution:uncommited_events(Exec8),
    %
    [{program_initialized,       _TS1, {program, []}},
     {program_location_adjusted, _TS2, {1,2}, init},
     {program_direction_adjusted,_TS3, south, init},
     {program_moved,             _TS4, {1,3}},
     {program_moved,             _TS5, {1,4}},
     {program_moved,             _TS6, {1,5}},
     {program_left_rotated,      _TS7, east},
     {program_moved,             _TS8, {2,5}}] = Events.


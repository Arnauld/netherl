-module(ne_program_execution_test).
-include("netherl.hrl").
-include_lib("eunit/include/eunit.hrl").

usecase_001_test() ->
    World = #world{blocks=[]},
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
    Types  = ne_events:extract_types(Events),
    ?assertEqual([program_initialized,
                  program_location_adjusted,
                  program_direction_adjusted,
                  program_moved,
                  program_moved,
                  program_moved,
                  program_left_rotated,
                  program_moved], Types).
    

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
    



prepare_exec(Location, Direction) ->
    World = #world{blocks=[]},
    Exec0 = ne_program_execution:new("d06f00d"),
    Exec1 = ne_program_execution:init_program(Exec0, #program{}),
    Exec2 = ne_program_execution:locate_at(Exec1, Location),
    Exec3 = ne_program_execution:look_at(Exec2, Direction),
    % consume uncommitted events...
    Exec4 = ne_program_execution:process_unsaved_changes(Exec3, fun(Id, Events) -> ignored end),
    {World, Exec4}.


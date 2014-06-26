-module(ne_usecases_test).
-include("asserts.hrl").
-include_lib("eunit/include/eunit.hrl").


simple_square_world_1_test() ->
    %
    % GIVEN
    %

    % Fix timestamp
    TS = 17,
    meck:new(ne_util),
    meck:expect(ne_util, timestamp, fun() -> TS end),
    
    %
    World = simple_square_world(),
    Prgm  = ne_program:new_program([{main, [fn1, fn1, fn1, fn1]},
                                    {fn1, [mov, mov, mov, mov, rotr]}]),
    Exec3 = init_execution(Prgm, {1,1}, east),
    %
    % WHEN
    %
    ExecN = next_instr_n_times(Exec3, 20, World),
    % discard mock
    meck:unload(ne_util),
    %
    % THEN
    %
    ActualEvents = ne_program_execution:uncommited_events(ExecN),
    ExpectedEvents = [
      {program_initialized,         TS, {program,[{fn1,[mov,mov,mov,mov,rotr]},
                                                  {main,[fn1,fn1,fn1,fn1]}]}},
      {program_location_adjusted,   TS,{1,1},init},
      {program_direction_adjusted,  TS,east,init},
      {program_instr_prepared,      TS,{mov,fn1,2,[{main,2}]}},
      {program_moved,               TS,{2,1}},
      {program_instr_prepared,      TS,{mov,fn1,3,[{main,2}]}},
      {program_moved,               TS,{3,1}},
      {program_instr_prepared,      TS,{mov,fn1,4,[{main,2}]}},
      {program_moved,               TS,{4,1}},
      {program_instr_prepared,      TS,{mov,fn1,5,[{main,2}]}},
      {program_moved,               TS,{5,1}},
      {program_instr_prepared,      TS,{rotr,fn1,6,[{main,2}]}},
      {program_right_rotated,       TS,south},
      {program_instr_prepared,      TS,{mov,fn1,2,[{main,3}]}},
      {program_moved,               TS,{5,2}},
      {program_instr_prepared,      TS,{mov,fn1,3,[{main,3}]}},
      {program_moved,               TS,{5,3}},
      {program_instr_prepared,      TS,{mov,fn1,4,[{main,3}]}},
      {program_moved,               TS,{5,4}},
      {program_instr_prepared,      TS,{mov,fn1,5,[{main,3}]}},
      {program_moved,               TS,{5,5}},
      {program_instr_prepared,      TS,{rotr,fn1,6,[{main,3}]}},
      {program_right_rotated,       TS,west},
      {program_instr_prepared,      TS,{mov,fn1,2,[{main,4}]}},
      {program_moved,               TS,{4,5}},
      {program_instr_prepared,      TS,{mov,fn1,3,[{main,4}]}},
      {program_moved,               TS,{3,5}},
      {program_instr_prepared,      TS,{mov,fn1,4,[{main,4}]}},
      {program_moved,               TS,{2,5}},
      {program_instr_prepared,      TS,{mov,fn1,5,[{main,4}]}},
      {program_moved,               TS,{1,5}},
      {program_instr_prepared,      TS,{rotr,fn1,6,[{main,4}]}},
      {program_right_rotated,       TS,north},
      {program_instr_prepared,      TS,{mov,fn1,2,[{main,5}]}},
      {program_moved,               TS,{1,4}},
      {program_instr_prepared,      TS,{mov,fn1,3,[{main,5}]}},
      {program_moved,               TS,{1,3}},
      {program_instr_prepared,      TS,{mov,fn1,4,[{main,5}]}},
      {program_moved,               TS,{1,2}},
      {program_instr_prepared,      TS,{mov,fn1,5,[{main,5}]}},
      {program_moved,               TS,{1,1}},
      {program_instr_prepared,      TS,{rotr,fn1,6,[{main,5}]}},
      {program_right_rotated,       TS,east}],
    assert_lists_are_equal(ExpectedEvents, ActualEvents).


simple_square_world_with_store_and_load_from_history_test() ->
    %
    start_store(),
    try
        World = simple_square_world(),
        Prgm  = ne_program:new_program([{main, [fn1, fn1, fn1, fn1]},
                                        {fn1, [mov, mov, mov, mov, rotr]}]),
        Exec0 = init_execution(Prgm, {1,1}, east),
        Id    = ne_program_execution:id(Exec0),
        Exec1 = next_instr_n_times(Exec0, 20, World),
        Exec2 = ne_program_execution:process_unsaved_changes(Exec1, fun(Id, Events) -> 
            ne_store:append_events(Id, Events)
        end),
        %
        ExecA = ne_program_execution:new(Id),
        ExecB = ne_program_execution:load_from_history(ExecA, ne_store:get_events(Id)),
        %
        ?assertEqual(Exec2, ExecB)
    after
        stop_store()
    end.

%% ------------------------------------------------------------------
%% Utilities Functions
%% ------------------------------------------------------------------


%%    1 2 3 4 5
%% 1  S X X X X
%% 2  X       X
%% 3  X       X
%% 4  X       X
%% 5  X X X X X 
simple_square_world() ->
    BlockCoords = [ {1,1}, {2,1}, {3,1}, {4,1}, {5,1},
                    {1,2},                      {5,2},
                    {1,3},                      {5,3},
                    {1,4},                      {5,4},
                    {1,5}, {2,5}, {3,5}, {4,5}, {5,5}],
    ListOfBlocks = lists:map(fun(C) -> {C, ne_block:new_block()} end, BlockCoords),
    ne_world:new_world(ListOfBlocks).


%% ---------------------
%% init_execution
%% ---------------------

init_execution(Prgm, Location, Direction) ->
    Exec0 = ne_program_execution:new("d06f00d"),
    Exec1 = ne_program_execution:init_program(Exec0, Prgm),
    Exec2 = ne_program_execution:locate_at(Exec1, Location),
    Exec3 = ne_program_execution:look_at(Exec2, Direction),
    Exec3.
    
%%
%%
%%
next_instr_n_times(Exec, N, World) ->
    Iter  = lists:seq(1, N),
    ExecN = lists:foldl(fun(I, Ex) -> 
        ne_program_execution:next_instr(Ex, World)
    end, Exec, Iter),
    ExecN.


%% ---------------------
%% asserts
%% ---------------------

assert_lists_are_equal(ExpectedList, ActualList) ->
    Len1 = length(ExpectedList),
    Len2 = length(ActualList),
    Len  = lists:min([Len1, Len2]),
    lists:foreach(fun(Index) -> 
        Expected = lists:nth(Index, ExpectedList),
        Actual   = lists:nth(Index, ActualList),
        % ?debugFmt("#~p: ~p =?= ~p ~n", [Index, ActualEvent, ExpectedEvent]),
        ?assertEqual(Expected, Actual) 
    end, lists:seq(1, Len)),
    if
        Len1 > Len2 ->
            ?fail("Missing element from actual list");
        Len2 > Len1 ->
            ?fail("More elements than expected");
        true ->
            ok
    end.

%% ---------------------
%% stop/start store
%% ---------------------

start_store() ->
    ne_store:open_store(),
    ne_store:start_link().

stop_store() ->
    ne_store:stop(true),
    ne_store:close_store().

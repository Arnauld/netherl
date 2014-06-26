-module(ne_usecases_test).
-include("netherl.hrl").
-include_lib("eunit/include/eunit.hrl").



%%    1 2 3 4 5
%% 1  S X X X X
%% 2  X       X
%% 3  X       X
%% 4  X       X
%% 5  X X X X X 
simple_world_1_test() ->
    %%
    %% GIVEN
    %%
    %% Fix timestamp
    TS = 17,
    meck:new(ne_util),
    meck:expect(ne_util, timestamp, fun() -> TS end),
    %
    BlockCoords = [ {1,1}, {2,1}, {3,1}, {4,1}, {5,1},
                    {1,2},                      {5,2},
                    {1,3},                      {5,3},
                    {1,4},                      {5,4},
                    {1,5}, {2,5}, {3,5}, {4,5}, {5,5}],
    ListOfBlocks = lists:map(fun(C) -> {C, ne_block:new_block()} end, BlockCoords),
    World = ne_world:new_world(ListOfBlocks),
    Prgm  = ne_program:new_program([{main, [fn1, fn1, fn1, fn1]},
                                    {fn1, [mov, mov, mov, mov, rotr]}]),
    Exec0 = ne_program_execution:new("d06f00d"),
    Exec1 = ne_program_execution:init_program(Exec0, Prgm),
    Exec2 = ne_program_execution:locate_at(Exec1, {1,1}),
    Exec3 = ne_program_execution:look_at(Exec2, east),
    %
    % WHEN
    %
    Iter  = lists:seq(1, 20),
    ExecN = lists:foldl(fun(I, Exec) -> 
        ne_program_execution:next_instr(Exec, World)
    end, Exec3, Iter),
    %
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
      {program_instr_prepared,      TS,mov,fn1,2,[{main,2}]},
      {program_moved,               TS,{2,1}},
      {program_instr_prepared,      TS,mov,fn1,3,[{main,2}]},
      {program_moved,               TS,{3,1}},
      {program_instr_prepared,      TS,mov,fn1,4,[{main,2}]},
      {program_moved,               TS,{4,1}},
      {program_instr_prepared,      TS,mov,fn1,5,[{main,2}]},
      {program_moved,               TS,{5,1}},
      {program_instr_prepared,      TS,rotr,fn1,6,[{main,2}]},
      {program_right_rotated,       TS,south},
      {program_instr_prepared,      TS,mov,fn1,2,[{main,3}]},
      {program_moved,               TS,{5,2}},
      {program_instr_prepared,      TS,mov,fn1,3,[{main,3}]},
      {program_moved,               TS,{5,3}},
      {program_instr_prepared,      TS,mov,fn1,4,[{main,3}]},
      {program_moved,               TS,{5,4}},
      {program_instr_prepared,      TS,mov,fn1,5,[{main,3}]},
      {program_moved,               TS,{5,5}},
      {program_instr_prepared,      TS,rotr,fn1,6,[{main,3}]},
      {program_right_rotated,       TS,west},
      {program_instr_prepared,      TS,mov,fn1,2,[{main,4}]},
      {program_moved,               TS,{4,5}},
      {program_instr_prepared,      TS,mov,fn1,3,[{main,4}]},
      {program_moved,               TS,{3,5}},
      {program_instr_prepared,      TS,mov,fn1,4,[{main,4}]},
      {program_moved,               TS,{2,5}},
      {program_instr_prepared,      TS,mov,fn1,5,[{main,4}]},
      {program_moved,               TS,{1,5}},
      {program_instr_prepared,      TS,rotr,fn1,6,[{main,4}]},
      {program_right_rotated,       TS,north},
      {program_instr_prepared,      TS,mov,fn1,2,[{main,5}]},
      {program_moved,               TS,{1,4}},
      {program_instr_prepared,      TS,mov,fn1,3,[{main,5}]},
      {program_moved,               TS,{1,3}},
      {program_instr_prepared,      TS,mov,fn1,4,[{main,5}]},
      {program_moved,               TS,{1,2}},
      {program_instr_prepared,      TS,mov,fn1,5,[{main,5}]},
      {program_moved,               TS,{1,1}},
      {program_instr_prepared,      TS,rotr,fn1,6,[{main,5}]},
      {program_right_rotated,       TS,east}],
    Len = length(ExpectedEvents),
    lists:foreach(fun(Index) -> 
        ExpectedEvent = lists:nth(Index, ExpectedEvents),
        ActualEvent   = lists:nth(Index, ActualEvents),
        % ?debugFmt("#~p: ~p =?= ~p ~n", [Index, ActualEvent, ExpectedEvent]),
        ?assertEqual(ExpectedEvent, ActualEvent) 
    end, lists:seq(1, Len)).

-module(ne_world_test).
-include("netherl.hrl").
-include_lib("eunit/include/eunit.hrl").

simple_usecase_test() ->
	Block0   = ne_block:new_block(),
	Location = {1, 3},
	%%
	World0 = ne_world:new_world(),
	?assertEqual(false, ne_world:block_exists(World0, Location)),
	World1 = ne_world:add_block(World0, Location, Block0),
	?assertEqual(true, ne_world:block_exists(World1, Location)),
	?assertEqual(Block0, ne_world:block_at(World1, Location)).
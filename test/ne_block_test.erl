-module(ne_block_test).
-include_lib("eunit/include/eunit.hrl").

can_move_to_should_accepts_by_default_test() ->
	Block = ne_block:new_block(),
	?assertEqual(true, ne_block:can_move_to(Block)).
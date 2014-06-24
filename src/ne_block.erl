-module(ne_block).
-include("netherl.hrl").

-record(block, {occupied_by = nobody}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_block/0]).
-export([can_move_to/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new_block() ->
    #block{}.

can_move_to(Block) ->
	OccupiedBy = Block#block.occupied_by,
	case OccupiedBy of
		nobody -> true;
		_ -> false
    end.

-module(ne_block).
-include("netherl.hrl").

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
	OCCUPIED_BY = Block#block.occupied_by,
	if
		OCCUPIED_BY == nobody -> true;
		true -> false
    end.

-module(ne_world).
-include("netherl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_world/0]).
-export([block_exists/2, add_block/3, block_at/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new_world() ->
    #world{blocks = dict:new()}.

block_exists(World, Location) ->
    Key = location_to_key(Location),
    Blocks = World#world.blocks,
    dict:is_key(Key, Blocks).

add_block(World, Location, Block) ->
    Key = location_to_key(Location),
    Blocks = World#world.blocks,
    NewBlocks = dict:store(Key, Block, Blocks),
    World#world{blocks=NewBlocks}.

block_at(World, Location) ->
    Key = location_to_key(Location),
    Blocks = World#world.blocks,
    {ok, Block} = dict:find(Key, Blocks),
    Block.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

location_to_key(Location) ->
    io_lib:format("w~p_~p", Location).

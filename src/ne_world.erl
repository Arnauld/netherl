-module(ne_world).
-include("netherl.hrl").

-record(world, { blocks }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_world/0, new_world/1]).
-export([block_exists/2, can_move_to/2,
         add_block/3, block_at/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new_world() ->
    #world{blocks = dict:new()}.


new_world(ListOfBlocks) ->
    Blocks = lists:foldl(fun({Loc, Block}, Dict) ->
        Key = location_to_key(Loc),
        dict:store(Key, Block, Dict)
    end, dict:new(), ListOfBlocks),
    #world{blocks = Blocks}.

can_move_to(World, Location) ->
    Key = location_to_key(Location),
    Blocks = World#world.blocks,
    case dict:find(Key, Blocks) of
        {ok, Block} -> ne_block:can_move_to(Block);
        error -> false
    end.


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

location_to_key(Loc) ->
    Loc.

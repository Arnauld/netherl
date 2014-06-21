-module(ne_commands).

-include("netherl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([execute/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

execute(move_forward, {World, Exec, _Prgm}) ->
  {Direction, X0, Y0} = direction_XY(Exec),
  [DX, DY]  = offset_for_direction(Direction),
  NewLocation = [X0+DX, Y0+DY],
  [{program_moved, NewLocation}].


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

direction_XY(Exec) ->
  Direction = Exec#execution.direction,
  [X0, Y0]  = Exec#execution.location,
  {Direction, X0, Y0}.


offset_for_direction(north) -> [0, -1];
offset_for_direction(south) -> [0, +1];
offset_for_direction(east)  -> [+1, 0];
offset_for_direction(west)  -> [-1, 0].

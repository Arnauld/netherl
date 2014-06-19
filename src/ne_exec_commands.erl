-module(ne_exec_commands).

-include("netherl.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([move_forward/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

move_forward({World, Exec}) ->
  Direction = Exec#execution.direction,
  [X0, Y0] = Exec#execution.location,
  [DX, DY] = offset_for_direction(Direction),
  NewLocation = [X0+DX, Y0+DY],
  {World, Exec#execution{location=NewLocation}}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

offset_for_direction(north) -> [0, -1];
offset_for_direction(south) -> [0, +1];
offset_for_direction(east)  -> [+1, 0];
offset_for_direction(west)  -> [-1, 0].

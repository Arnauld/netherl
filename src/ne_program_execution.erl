-module(ne_program_execution).

-include("netherl.hrl").
-define(PROCESS_TIME_OUT, 45000).

-record(exec, {id, 
                prgm, 
                prgm_idx  = 0, 
                prgm_fn   = main,
                location  = {0,0},
                direction = north,
                uncommited_events=[]}).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/1, 
         process_unsaved_changes/2,
         uncommited_events/1,
         load_from_history/2]).

-export([init_program/2,
         locate_at/2,
         locate_at/3,
         look_at/2,
         look_at/3,
         move_forward/2, 
         rotate_left/1, 
         rotate_right/1]).

-export([location/1,
         direction/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------
%% Event Sourcing...
%% ------------------

new(Id) ->
    #exec{id = Id}.

process_unsaved_changes(Exec, Saver) ->
    Id = Exec#exec.id,
    Events = uncommited_events(Exec),
    Saver(Id, Events),
    NewExec = Exec#exec{uncommited_events=[]},
    NewExec.

uncommited_events(Exec) ->
    lists:reverse(Exec#exec.uncommited_events).

load_from_history(Exec, []) ->
    Exec;
load_from_history(Exec, [Event|Rest]) ->
    NewExec = apply_event(Exec, Event),
    load_from_history(NewExec, Rest).


%% ------------------
%% Domain - State
%% ------------------

location(Exec) ->
    Exec#exec.location.

direction(Exec) ->
    Exec#exec.direction.

%% ------------------
%% Domain - Action
%% ------------------

locate_at(Exec, Location) ->
    locate_at(Exec, Location, init).

locate_at(Exec, Location, Reason) ->
    apply_new_event(Exec, {program_location_adjusted, timestamp(), Location, Reason}).

look_at(Exec, Direction) ->
    look_at(Exec, Direction, init).

look_at(Exec, Direction, Reason) when Direction==north; 
                                      Direction==south; 
                                      Direction==east;
                                      Direction==west ->
    apply_new_event(Exec, {program_direction_adjusted, timestamp(), Direction, Reason});

look_at(_Exec, Direction, _Reason) ->
    throw({unsupported_direction, Direction}).


init_program(Exec, Prgm) ->
    apply_new_event(Exec, {program_initialized, timestamp(), Prgm}).


move_forward(Exec, World) ->
    Direction = Exec#exec.direction,
    {X0, Y0}  = Exec#exec.location,
    {DX, DY}  = offset_for_direction(Direction),
    NewLocation = {X0+DX, Y0+DY},
    case ne_world:can_move_to(World, NewLocation) of
        true  -> apply_new_event(Exec, {program_moved, timestamp(), NewLocation});
        false -> throw({illegal_move, NewLocation})
    end.


rotate_left(Exec) ->
    Direction = Exec#exec.direction,
    NewDirection = case Direction of
        north -> west;
        west  -> south;
        south -> east;
        _     -> north
    end,
    apply_new_event(Exec, {program_left_rotated, timestamp(), NewDirection}).

    
rotate_right(Exec) ->
    Direction = Exec#exec.direction,
    NewDirection = case Direction of
        north -> east;
        east  -> south;
        south -> west;
        _     -> north
    end,
    apply_new_event(Exec, {program_right_rotated, timestamp(), NewDirection}).
    
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

apply_new_event(State, Event) ->
    NewExec = apply_event(State, Event),
    CombinedChanges = [Event] ++ NewExec#exec.uncommited_events,
    NewExec#exec{uncommited_events=CombinedChanges}.

%%
%%
%%

apply_event(Exec, {program_location_adjusted, _Timestamp, Location, _Reason}) ->
    Exec#exec{location=Location};

apply_event(Exec, {program_direction_adjusted, _Timestamp, Direction, _Reason}) ->
    Exec#exec{direction=Direction};

apply_event(Exec, {program_right_rotated, _Timestamp, Direction}) ->
    Exec#exec{direction=Direction};

apply_event(Exec, {program_left_rotated, _Timestamp, Direction}) ->
    Exec#exec{direction=Direction};

apply_event(Exec, {program_initialized, _Timestamp, Prgm}) ->
    Exec#exec{prgm = Prgm};

apply_event(Exec, {program_moved, _Timestamp, NewLocation}) ->
    Exec#exec{location=NewLocation}.

%% ------------------
%% Domain
%% ------------------

offset_for_direction(north) -> {0, -1};
offset_for_direction(south) -> {0, +1};
offset_for_direction(east)  -> {+1, 0};
offset_for_direction(west)  -> {-1, 0}.

%% ------------------
%% Misc
%% ------------------


timestamp() -> 
    ne_util:timestamp(). 

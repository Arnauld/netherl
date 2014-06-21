-module(ne_program_execution).

-include("netherl.hrl").
-define(PROCESS_TIME_OUT, 45000).
-record(state, {id, prgm, execution, changes=[]}).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new/0, init/4, process_unsaved_changes/2, load_from_history/2]).
-export([move_forward/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% ------------------
%% Event Sourcing...
%% ------------------

new() ->
    spawn(fun() -> start_loop() end).

init(Pid, Id, Prgm, Execution) ->
    Pid ! {attempt_command, {init, Id, Prgm, Execution}}.

process_unsaved_changes(Pid, Saver) ->
    Pid ! {process_unsaved_changes, Saver}.

load_from_history(Pid, Events) ->
    Pid ! {load_from_history, Events}.

%% ------------------
%% Domain
%% ------------------

move_forward(Pid, World) ->
    Pid ! {attempt_command, {move_forward, World}}.




%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------
%% Loop
%% ------------------

start_loop() -> 
    State = #state{},
    loop(State).


loop(State) ->
    %% error_logger:info_msg("Process ~p state:[~p]~n", [self(), State]),
    receive 
        {apply_event, Event} ->
            NewState = apply_event(State, Event),
            loop(NewState); 
        {attempt_command, Command} ->
            NewState = attempt_command(State, Command),
            loop(NewState);
        {process_unsaved_changes, Saver} ->
            Id = State#state.id,
            Saver(Id, lists:reverse(State#state.changes)),
            NewState = State#state{changes=[]},
            loop(NewState);
        {load_from_history, Events} ->
            NewState = apply_many_events(#state{}, Events),
            loop(NewState);
        Unknown -> 
            error_logger:warning_msg("Received unknown message (~p)~n", [Unknown]),
            loop(State)
        after ?PROCESS_TIME_OUT ->
            shutting_down
    end.


%% ------------------
%% Commands
%% ------------------

attempt_command(State, {init, Id, Prgm, Execution}) ->
    apply_new_event(State, {initialized, timestamp(), Id, Prgm, Execution});

attempt_command(State, {move_forward, World}) ->
    Execution = State#state.execution,
    Direction = Execution#execution.direction,
    {X0, Y0}  = Execution#execution.location,
    {DX, DY}  = offset_for_direction(Direction),
    NewLocation = {X0+DX, Y0+DY},
    apply_new_event(State, {program_moved, timestamp(), NewLocation}).


%% ------------------
%% Events
%% ------------------

apply_new_event(State, Event) ->
    NewState = apply_event(State, Event),
    CombinedChanges = [Event] ++ NewState#state.changes,
    NewState#state{changes=CombinedChanges}.

apply_many_events(State, []) ->
    State;
apply_many_events(State, [Event|Rest]) ->
    NewState = apply_event(State, Event),
    apply_many_events(NewState, Rest).

apply_event(State, {initialized, _Timestamp, Id, Prgm, Execution}) ->
    State#state{id = Id,
                execution = Execution,
                prgm = Prgm};

apply_event(State, {program_moved, _Timestamp, NewLocation}) ->
    Execution = State#state.execution,
    NewExecution = Execution#execution{location=NewLocation},
    State#state{execution=NewExecution}.

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
    erlang:localtime().

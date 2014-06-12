-module(ne_store).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(TABLE_ID, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([open_store/0, close_store/0]).
-export([start_link/0, stop/0]).
-export([append_events/2, get_events/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

open_store() ->
    ets:new(?TABLE_ID, [public, named_table]),
    ok.

close_store() ->
    ets:delete(?TABLE_ID).

stop() ->
    gen_server:cast(?SERVER, shutdown).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

append_events(Key, Events) ->
    gen_server:cast(?SERVER, {append_events, Key, Events}).

get_events(Key) ->
    gen_server:call(?SERVER, {get_events, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    % If it is necessary to clean up before termination, 
    % the shutdown strategy must be a timeout value and 
    % the gen_server must be set to trap exit signals in
    % the init function.
    % When ordered to shutdown, the gen_server will then 
    % call the callback function terminate(shutdown, State):
    process_flag(trap_exit, true),
    {ok, Args}.

%%
%% handle_call/3
%% 
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call({get_events, Key}, _From, State) ->
    Events = lists:reverse(get_raw_events(Key)),
    {reply, Events, State}.


%%
%% handle_cast/2
%% 
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({append_events, Key, Events}, State) ->
    StoredEvents = get_raw_events(Key),
    NewEvents = lists:reverse(Events),
    CombinedEvents = NewEvents ++ StoredEvents,
    ets:insert(?TABLE_ID, {Key, CombinedEvents}),
%%    lists:foreach(fun (Event) -> bus:publish_event(Event) end, NewEvents),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% handle_info/2
%% 
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_raw_events(Key) ->
    case ets:lookup(?TABLE_ID, Key) of
        [{Key, Events}] -> Events;
        [] -> []
    end.

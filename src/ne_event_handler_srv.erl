-module(ne_event_handler_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(BUFFER_SIZE, 10).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1, stop/0, sync_stop/0]).
-export([push_event/1, last_events/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    start_link(?BUFFER_SIZE).

start_link(BufferSize) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [BufferSize], []).

stop() ->
    gen_server:cast(?SERVER, stop).

sync_stop() ->
    gen_server:call(?SERVER, stop).

push_event(Event) ->
    gen_server:cast(?SERVER, {push_event, Event}).

last_events() ->
    gen_server:call(?SERVER, {last_events}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([BufferSize]) ->
    % If it is necessary to clean up before termination, 
    % the shutdown strategy must be a timeout value and 
    % the gen_server must be set to trap exit signals in
    % the init function.
    % When ordered to shutdown, the gen_server will then 
    % call the callback function terminate(shutdown, State):
    process_flag(trap_exit, true),
    {ok, {0, BufferSize, []}}.

%%
%% handle_call/3
%%
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call({last_events}, _From, State) ->
    {_Size, _MaxSize, Events} = State,
    {reply, Events, State}; %% {response_code, Response, NewState}
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%
%% handle_cast/2
%% 
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({push_event, Event}, State) ->
    {Size, MaxSize, LastEvents} = State,
    Updated = if 
                Size == MaxSize ->
                    {Size, MaxSize, [Event | drop_last(LastEvents)]};
                true ->
                    {Size+1, MaxSize, [Event|LastEvents]}
              end,
    {noreply, Updated};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% handle_info/2
%% 
handle_info(_Info, State) ->
    {noreply, State}.

%%
%% terminate/2
%% 
terminate(_Reason, _State) ->
    ok.

%%
%% code_change/3
%% 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% drop the last element from the list
drop_last([_])   -> [];
drop_last([H|T]) -> [H | drop_last(T)].


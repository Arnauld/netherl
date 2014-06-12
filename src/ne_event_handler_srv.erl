-module(ne_event_handler_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(BUFFER_SIZE, 10).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start_link/1, stop/0]).
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
    io:format("ne_event_handler_srv:init() [~p]~n", [BufferSize]),
    {ok, {0, BufferSize, []}}.

%%
%% handle_call/3
%%
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call({last_events}, _From, State) ->
    {_Size, _MaxSize, Events} = State,
    Reply = Events,
    io:format("ne_event_handler_srv:handle_call:last_events ~p ~n", [Reply]),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    io:format("ne_event_handler_srv:handle_call:unknown: ~p~n", [Request]),
    {reply, ok, State}.

%%
%% handle_cast/2
%% 
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({push_event, Event}, State) ->
    try
        erlang:display("ne_event_handler_srv:push_event"),
        erlang:display(Event),
        erlang:display(State),
        erlang:display("::"),
        
        io:format("ne_event_handler_srv:handle_cast:push_event() before: ~p~n", [State]),
        {Size, MaxSize, LastEvents} = State,
        io:format("ne_event_handler_srv:handle_cast:push_event() before: [~p, ~p, ~p]~n", [Size, MaxSize, LastEvents]),
        Updated = if 
                    Size == MaxSize ->
                        {Size, MaxSize, [Event | drop_last(LastEvents)]};
                    true ->
                        {Size+1, MaxSize, [Event|LastEvents]}
                  end,
        io:format("ne_event_handler_srv:handle_cast:push_event() after: ~p => ~p ~n", [Event, Updated]),
        {noreply, Updated}
    catch
        What:Reason ->
            erlang:display("ne_event_handler_srv:Ooooooooouuch!"),
            erlang:display(What),
            erlang:display(Reason),
            erlang:display(erlang:get_stacktrace())
    end;

handle_cast(Msg, State) ->
    io:format("ne_event_handler_srv:handle_cast:unknown: ~p~n", [Msg]),
    {noreply, State}.

%%
%% handle_info/2
%% 
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    erlang:display("ne_event_handler_srv:terminate"),
    erlang:display(self()),
    erlang:display(Reason),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% drop the last element from the list
drop_last([_])   -> [];
drop_last([H|T]) -> [H | drop_last(T)].


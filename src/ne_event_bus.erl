-module(ne_event_bus).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/1]).
-export([register_handler/2, unregister_handler/2]).
-export([publish_event/1, sync_publish_event/1]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_event:start_link({local, ?SERVER}).

stop(_Async) ->
    not_implemented.

publish_event(Event) ->
    gen_event:notify(?SERVER, Event).

sync_publish_event(Event) ->
    gen_event:sync_notify(?SERVER, Event).

register_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

unregister_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

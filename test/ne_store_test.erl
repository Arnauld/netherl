-module(ne_store_test).

-include_lib("eunit/include/eunit.hrl").


%% ------------------------------------------------------------------
%% Utilities Functions
%% ------------------------------------------------------------------

start_store() ->
    ne_store:open_store(),
    ne_store:start_link().

stop_store() ->
    ne_store:stop(),
    ne_store:close_store().

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

get_events_should_returns_events_in_order_test() ->
    start_store(),
    try
        ne_store:append_events(id1, [e1, e2]),
        ?assertEqual([e1, e2], ne_store:get_events(id1)),
        ok
    after
        stop_store()
    end.

get_events_should_returns_events_in_order_when_append_is_called_several_times_test() ->
    start_store(),
    try
        ne_store:append_events(id1, [e1, e2]),
        ne_store:append_events(id1, [e3]),
        ne_store:append_events(id1, [e4, e5]),
        ?assertEqual([e1, e2, e3, e4, e5], ne_store:get_events(id1)),
        ok
    after
        stop_store()
    end.

get_events_should_returns_empty_list_of_events_when_stream_id_is_unknown_test() ->
    start_store(),
    try
        ?assertEqual([], ne_store:get_events(unknown_id)),
        ok
    after
        stop_store()
    end.

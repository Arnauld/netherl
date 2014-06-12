-module(ne_event_bus_test).

-include_lib("eunit/include/eunit.hrl").


%% ------------------------------------------------------------------
%% Utilities Functions
%% ------------------------------------------------------------------

start_bus() ->
    ne_event_bus:start_link(),
    ne_event_handler_srv:start_link(),
    ne_event_handler:register().

stop_bus() ->
    ne_event_handler:unregister(),
    ne_event_handler_srv:stop(true),
    ne_event_bus:stop(true).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

publish_event_should_propagate_to_registered_handler_test() ->
    start_bus(),
    try
        ne_event_bus:publish_event(e1),
        ne_event_bus:sync_publish_event(e2),
        ?assertEqual([e2, e1], ne_event_handler:last_events()),
        ok
    after
        stop_bus()
    end.

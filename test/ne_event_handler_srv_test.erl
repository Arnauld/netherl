-module(ne_event_handler_srv_test).

-include_lib("eunit/include/eunit.hrl").

handler_should_only_keep_the_last_N_events_test() ->
    ne_event_handler_srv:start_link(2),
    try
        ne_event_handler_srv:push_event(e1),
        ne_event_handler_srv:push_event(e2),
        ne_event_handler_srv:push_event(e3),
        ne_event_handler_srv:push_event(e4),
        ?assertEqual([e4, e3], ne_event_handler_srv:last_events()),
        ok
    after
        Res = ne_event_handler_srv:stop()
    end.

handler_should_return_empty_list_when_no_events_have_been_pushed_test() ->
    ne_event_handler_srv:start_link(2),
    try
        ?assertEqual([], ne_event_handler_srv:last_events()),
        ok
    after
        Res = ne_event_handler_srv:stop()
    end.



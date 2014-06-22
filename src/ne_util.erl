-module(ne_util).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([timestamp/0]). 

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

timestamp() ->
	timestamp_millis().

timestamp_millis() -> 
    {Mega,Sec,Micro} = erlang:now(),
    Micros = (Mega*1000000+Sec)*1000000+Micro,
    Millis = erlang:trunc(Micros/1000),
    Millis.

timestamp_atom() -> 
    Now = erlang:now(),
    {_, _, Micros} = Now, 
    Millis = erlang:trunc(Micros/1000),
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(Now), 
    Fmt = io_lib:format("utc~4..0w~2..0w~2..0w_~2..0w~2..0w~2..0w_~p", 
                  [YY, MM, DD, Hour, Min, Sec, Millis]),
    list_to_atom(lists:flatten(Fmt)). 
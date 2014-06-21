-module(ne_events_test).
-include_lib("eunit/include/eunit.hrl").

extract_types_from_a_one_sized_list_test() ->
	?assertEqual([program_created], ne_events:extract_types([{program_created, erlang:localtime(), arg1}])),
	?assertEqual([program_created], ne_events:extract_types([{program_created, erlang:localtime(), arg1, arg2}])).

extract_types_from_a_two_sized_list_test() ->
	?assertEqual([program_created, program_launched], 
				 ne_events:extract_types([{program_created, erlang:localtime(), arg1},
				 						  {program_launched, erlang:localtime(), arg1, arg2}])).

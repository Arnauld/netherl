-module(ne_program_test).

-include("netherl.hrl").
-include("asserts.hrl").
-include_lib("eunit/include/eunit.hrl").

new_should_contain_no_sequence_test() ->
	Prgm0 = ne_program:new_program(),
	?assertEqual(not_found, ne_program:find_sequence(Prgm0, main)),
	?assertEqual(not_found, ne_program:find_sequence(Prgm0, fn1)).


new_with_named_statements_should_allow_sequence_to_be_retrieved_test() ->
	Prgm0 = ne_program:new_program([{fn2, [mov, rotr]},
									{fn1, [rotl, mov]}]),
	?assertEqual(not_found, ne_program:find_sequence(Prgm0, main)),
	?assertEqual([rotl, mov], ne_program:find_sequence(Prgm0, fn1)).


declare_sequence_should_allow_sequence_to_be_retrieved_test() ->
	Prgm0 = ne_program:new_program(),
	Prgm1 = ne_program:declare_sequence(Prgm0, {fn1, [mov, mov, rotl, mov]}),
	Stmts = ne_program:find_sequence(Prgm1, fn1),
	?assertEqual([mov, mov, rotl, mov], Stmts).

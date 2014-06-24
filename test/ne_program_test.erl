-module(ne_program_test).

-include("netherl.hrl").
-include("asserts.hrl").
-include_lib("eunit/include/eunit.hrl").

usecase_001_test() ->
	Prgm0 = ne_program:new_program(),
	Prgm1 = ne_program:declare_sequence(Prgm0, {fn1, [mov, mov, rotl, mov]}),
	Stmts = ne_program:find_sequence(Prgm1, fn1),
	?assertEqual([mov, mov, rotl, mov], Stmts).

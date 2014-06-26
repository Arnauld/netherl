-module(ne_program_test).

-include("netherl.hrl").
-include("asserts.hrl").
-include_lib("eunit/include/eunit.hrl").

new_should_contain_no_sequence__test() ->
    Prgm0 = ne_program:new_program(),
    ?assertEqual(not_found, ne_program:find_sequence(Prgm0, main)),
    ?assertEqual(not_found, ne_program:find_sequence(Prgm0, fn1)).


new_with_named_statements_should_allow_sequence_to_be_retrieved__test() ->
    Prgm0 = ne_program:new_program([{fn2, [mov, rotr]},
                                    {fn1, [rotl, mov]}]),
    ?assertEqual(not_found, ne_program:find_sequence(Prgm0, main)),
    ?assertEqual({ok, [rotl, mov]}, ne_program:find_sequence(Prgm0, fn1)).


declare_sequence_should_allow_sequence_to_be_retrieved__test() ->
    Prgm0 = ne_program:new_program(),
    Prgm1 = ne_program:declare_sequence(Prgm0, {fn1, [mov, mov, rotl, mov]}),
    {ok, Stmts} = ne_program:find_sequence(Prgm1, fn1),
    ?assertEqual([mov, mov, rotl, mov], Stmts).

next_instruction_should_returns_the_next_instruction_in_the_same_sequence_initial_case__test() ->
    Prgm  = ne_program:new_program([{main, [mov, rotr, mov, rotr]}]),
    {Instr, Fn, Index, Stack} = ne_program:next_instr(Prgm, main, 1, []),
    ?assertEqual(mov, Instr),
    ?assertEqual(main, Fn),
    ?assertEqual(2, Index),
    ?assertEqual([], Stack).

next_instruction_should_returns_the_next_instruction_in_the_same_sequence_second_case__test() ->
    Prgm  = ne_program:new_program([{main, [mov, rotr, mov, rotr]}]),
    {Instr, Fn, Index, Stack} = ne_program:next_instr(Prgm, main, 2, []),
    ?assertEqual(rotr, Instr),
    ?assertEqual(main, Fn),
    ?assertEqual(3, Index),
    ?assertEqual([], Stack).

next_instruction_should_returns_the_next_instruction_in_the_same_sequence__before_end_of_program__test() ->
    Prgm  = ne_program:new_program([{main, [mov, rotr, mov, rotl]}]),
    {Instr, Fn, Index, Stack} = ne_program:next_instr(Prgm, main, 4, []),
    ?assertEqual(rotl, Instr),
    ?assertEqual(main, Fn),
    ?assertEqual(5, Index),
    ?assertEqual([], Stack).

next_instruction_should_returns_an__end_of_program__once_the_last_instr_is_reached__test() ->
    Prgm  = ne_program:new_program([{main, [mov, rotr, mov, rotr]}]),
    ?assertEqual(end_of_program, ne_program:next_instr(Prgm, main, 5, [])).

next_instruction_should_jump_to_seq_and_adjust_stack__test() ->
    Prgm  = ne_program:new_program([{main, [fun1, jmp]},
                                    {fun1, [rotr, mov, rotl]}]),
    {Instr, Fn, Index, Stack} = ne_program:next_instr(Prgm, main, 1, []),
    ?assertEqual(rotr, Instr),
    ?assertEqual(fun1, Fn),
    ?assertEqual(2, Index),
    ?assertEqual([{main,2}], Stack).

next_instruction_should__pop_and_adjust_stack_when_end_of_seq_is_reached__test() ->
    Prgm  = ne_program:new_program([{main, [fun1, jmp]},
                                    {fun1, [rotr, mov, rotl]}]),
    {Instr, Fn, Index, Stack} = ne_program:next_instr(Prgm, fun1, 4, [{main,2}]),
    ?assertEqual(jmp, Instr),
    ?assertEqual(main, Fn),
    ?assertEqual(3, Index),
    ?assertEqual([], Stack).



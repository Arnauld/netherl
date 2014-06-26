-module(ne_program).

-record(program, { stmts=[] }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_program/0, new_program/1]).
-export([declare_sequence/2, find_sequence/2, next_instr/4, instr_at/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new_program() ->
    new_program([]).

new_program(ListOfNamedStmts) ->
	Stmts = lists:foldl(fun({Name, Sequence}, Acc) ->
        [{Name, Sequence}] ++ Acc
    end, [], ListOfNamedStmts),
    #program{stmts = Stmts}.

declare_sequence(Program, {Name, Sequence}) ->
	OldStmts = Program#program.stmts,
	NewStmts = [{Name, Sequence}] ++ OldStmts,
	Program#program{stmts = NewStmts}.

find_sequence(Program, Name) ->
	find_sequence0(Program#program.stmts, Name).

instr_at(Program, Fn, Index) ->
	NamesSeqs = Program#program.stmts,
	{ok, Stmts} = find_sequence0(NamesSeqs, Fn),
	lists:nth(Index, Stmts).

next_instr(Program, Fn, Index, Stack) when Index > 0 ->
	NamesSeqs = Program#program.stmts,
	{ok, Stmts} = find_sequence0(NamesSeqs, Fn),
	if
		Index =< length(Stmts) ->
			Instr = lists:nth(Index, Stmts),
			case is_name_of_seq(NamesSeqs, Instr) of
				true ->
					next_instr(Program, Instr, 1, [{Fn, Index+1}] ++ Stack);
				_  ->
					{Instr, Fn, Index+1, Stack}
			end;
		true ->
			case Stack of
				[{PFn, PIndex} | Rest] ->
					next_instr(Program, PFn, PIndex, Rest);
				_ ->
					end_of_program
			end
	end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

is_name_of_seq([], Searched) ->
	false;
is_name_of_seq([{Name, _}| Rest], Searched) ->
	case Name of
		Searched -> true;
		_ -> is_name_of_seq(Rest, Searched)
	end.


find_sequence0([], Name) ->
	not_found;
find_sequence0([Seq | Rest], Name) ->
	{SeqName, SeqStmts} = Seq,
	case Name of
		SeqName -> {ok, SeqStmts};
		_ -> find_sequence0(Rest, Name)
	end.


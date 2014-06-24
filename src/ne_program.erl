-module(ne_program).

-record(program, { stmts=[] }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([new_program/0, new_program/1]).
-export([declare_sequence/2, find_sequence/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

new_program() ->
    new_program([]).

new_program(ListOfNamedStmts) ->
    #program{stmts = ListOfNamedStmts}.

declare_sequence(Program, {Name, Sequence}) ->
	OldStmts = Program#program.stmts,
	NewStmts = [{Name, Sequence}] ++ OldStmts,
	Program#program{stmts = NewStmts}.

find_sequence(Program, Name) ->
	find_sequence0(Program#program.stmts, Name).

find_sequence0([], Name) ->
	not_found;
find_sequence0([Seq | Rest], Name) ->
	{SeqName, SeqStmts} = Seq,
	case Name of
		SeqName -> SeqStmts;
		_ -> find_sequence0(Rest, Name)
	end.


%% Author: Francis Stephens
%% Created: 28 Jan 2008
%% Description: TODO: Add desciption to formula_parser
-module(formula_parser).

%%
%% Exported Functions
%%
-compile(export_all).
-export([get_test_formula/1,get_test_formulae/1,get_test_formulae/2]).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("sat_macros.hrl").
-include("test_records.hrl").

%%
%% API Functions
%%
get_test_formula(FilePath) ->
	case file:open(FilePath,read) of
		{ok,Val} -> 
			Test_Formula = parse_test_formula(list_lines(Val)),
			file:close(Val),
			Test_Formula;
		{error,Why} ->
			file:close(FilePath),
			{error,Why}
	end.

get_test_formulae(_FolderPath,_Extn) ->
	ok.

get_test_formulae(FolderPath) ->
	get_test_formulae(FolderPath,".cnf").

%%
%% Local Functions
%%

%%
%%	Reads every line from a file into a list
%%
%%	File_Val:
%%		An opened file
%%
list_lines(File_Val) ->
	case io:get_line(File_Val,'') of
		eof ->
			[];
		Line ->
			[Line|list_lines(File_Val)]
	end.

%%
%%	Parses each line from a formula file and returns a #test_formula
%%
parse_test_formula([Sat_Line|File_Lines]) ->
	parse_formula(remove_comments(File_Lines)),satisfiable=get_satisfiability(Sat_Line).


%%
%%	Reads the first line from a modified IBM cnf files
%%	The first line is assumed to indicate whether or not the formula is satisfiable
%%
get_satisfiability(Sat_Line) ->
	Sat_Match = regexp:first_match(Sat_Line, satisfiable_grep()),
	Unsat_Match = regexp:first_match(Sat_Line, unsatisfiable_grep()),
	Match_True = {match,1,length(Sat_Line)},
	if
		Sat_Match =:= Match_True
			-> satisfiable;
		Unsat_Match =:= Match_True
			-> unsatisfiable;
		true
			-> indetermined
	end.

%%
%%	Removes every line in the file which is a comment
%%	NB: The line indicating the satisfiability of this formula is a comment too.
%%
remove_comments(Lines) ->
	[Line|| Line <- Lines, regexp:first_match(Line,comment_grep()) =/= {match,1,length(Line)}].

%%
%%	Parses a list of strings where the first is a pair of space delimited integers
%%		The first integer indicates the number of clauses in the formula
%%		The second integer indicates the range 1 -> integer of valid variables names
%%		NB: This line is currently ignored
%%	The remaining lines are space delimited lists of integer variable names
%%	optionally prefixed by "-" indicating negation
%%	and terminating with the character "0"
%%		Returns a formula 
%%			#formula
%%
parse_formula([_Variable_Defs|Formula_Lines]) ->
	?FORMULA_DATA_STRUCTURE:def_2_formula(#formula_def{clause_defs=parse_formula_1(Formula_Lines)}),
    #formula_def{clause_defs=[]}.

parse_formula_1([]) -> [];

parse_formula_1([Line|Lines]) ->
	Lit_Tks = string:tokens(Line," "),
	New_Clause = #clause_def{literal_defs=parse_literals(Lit_Tks)},
	[New_Clause|parse_formula_1(Lines)].

%%
%%	Parses a single string reading in variable names 
%%	which are either negated (-) or not
%%	returns #clause
%%

%%	End of file marker
parse_literals(["0"]) -> [];

%%	End of line marker
parse_literals(["0\n"]) -> [];

parse_literals([L_String|L_Strings]) ->
	[make_literal(L_String)|parse_literals(L_Strings)].

%%
%%	Given a string which is made up of a single integer variable name
%%	and optionally prefixed by "-" indicating negation
%%	returns #literal
%%
make_literal([$-|L_String]) ->
	#literal_def{negation=false,l_name=L_String};
	
make_literal(L_String) ->
	#literal_def{negation=true,l_name=L_String}.
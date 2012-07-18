%% Author: Francis Stephens
%% Created: 1 Dec 2007
%% Description: Erlang Header file defining useful common records for communicating sat solver modules

%%
%%	Macros
%%

-define(NULL_LITERAL,null_literal).

%%
%% Records
%%

%%%
%%%	A literal is a variable, l_name, with a possible negation
%%%	The negation value acts as a truth switch as follows:
%%%		For a literal, L, where the variable l_name's assignment (val) != indetermined
%%%		truth(L) = (negation & val) v (~negation & ~val)
%%%
%%%	Literals can be read as classical boolean literals
%%%	For some literal named p 
%%%		where negation = true read p
%%%		where negation = false read ~p
%%%
%%% negation
%%%		true | false
%%%	l_name 
%%%		Atom()
-record(literal, {negation, l_name}).

%%%
%%%	A clause is a list of literals (implicitly joined by disjunctions)
%%%	A clause which is still present in a formula is assumed to contain no
%%%	literals evaluating to true (clauses containing true literals are removed)
%%%	Furthermore literals evaluating to false are removed from their clauses,
%%%	therefore any literals present are assumed to be indetermined.
%%%	
%%%	A clause containing no literals is considered empty and all of its
%%%	literals evaluate to false, this clause cannot be satisfied and will invalidate
%%%	the current partial assignment.
%%%
%%%	literals
%%%		[#literal]
-record(clause, {literals}).

%%%
%%%	A formula is a dictionary mapping variable names to ternary values
%%%	and a list of clauses containing variable names found as keys in variable_dict
%%%
%%%	variable_dict (of type dict:new())
%%%		Atom() >> true | false | indetermined
%%%	clauses
%%%		[#clause]
-record(formula, {variable_dict, clauses}).
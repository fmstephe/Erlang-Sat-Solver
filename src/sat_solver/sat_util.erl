%% Author: Francis Stephens
%% Created: 17 Jan 2008
%% Description: TODO: Add description to sat_util
-module(sat_util).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("sat_macros.hrl").

%%
%% Exported Functions
%%
-export([
			one_true_literal/2,all_false_literals/2,
			has_negation/3,truthifying_value/2,falsifying_value/2,
            negate_variable_assignment/1
		]).

%%
%% API Functions
%%

%%
%%	Returns true iff there exists at least one literal which evaluates to true
%%		Literals. A list of literals
%%			[#Literal]
%%		Formula. contains a dictionary mapping from literal names to ternary values
%%			#formula
%%
one_true_literal([],_Formula) -> false;

one_true_literal([Literal|Literals],Formula) ->
	case ?FORMULA_DATA_STRUCTURE:eval_literal(Literal,Formula) of
		true
			-> true;
		false 
			-> one_true_literal(Literals,Formula);
		indetermined 
			-> one_true_literal(Literals,Formula)
	end.

%%
%%	Returns true iff there do not exist any literals which evaluate to true
%%		Literals. A list of literals
%%			[#Literal]
%%		Formula. Contains a dictionary mapping from literal names to ternary values
%%			#formula
%%
all_false_literals([],_) -> true;

all_false_literals([Literal|Literals],Formula) ->
	case ?FORMULA_DATA_STRUCTURE:eval_literal(Literal,Formula) of
		true 
			-> false;
		indetermined 
			-> false;
		false 
			-> all_false_literals(Literals,Formula)
	end.

%%
%%	Return true iff the negation of the given literal is contained in the list of literals.
%%		Literal.  The literal whose negation we are searching for
%%			#literal
%%		Literals.  List of literals in which to search for a negation
%%			[#literal]
%%
has_negation(_Literal,[],_Formula)-> false;

has_negation(Literal,[Literal2|Literals],Formula) ->
	case is_negation(Literal,Literal2,Formula) of
		true 
			-> true;
		false 
			-> has_negation(Literal,Literals,Formula)
	end.

%%
%%	Returns true iff the two literals are the negation of each other.
%%	In propositional calculus written: if the literals are p and ~p respectively
%%		Literal1. First literal
%%			#literal
%%		Literal2. Second literal
%%			#literal
%%
is_negation(Literal1,Literal2,Formula) ->
	Variable1 = ?FORMULA_DATA_STRUCTURE:get_variable(Literal1,Formula),
	Variable2 = ?FORMULA_DATA_STRUCTURE:get_variable(Literal2,Formula),
	Negation1 = ?FORMULA_DATA_STRUCTURE:get_literal_negation(Literal1,Formula),
	Negation2 = ?FORMULA_DATA_STRUCTURE:get_literal_negation(Literal2,Formula),
	(Variable1 =:= Variable2) and (Negation1 =:= not Negation2).

%%
%%	Indicates the value which when assigned to this literal's
%%	variable name would make this literal evaluate to true
%%		Literal
%%			#literal
%%
truthifying_value(Literal,Formula) ->
	?FORMULA_DATA_STRUCTURE:get_literal_negation(Literal,Formula).

%%
%%	Indicates the value which when assigned to this literal's
%%	variable name would make this literal evaluate to false
%%		Literal
%%			#literal
%%
falsifying_value(Literal,Formula) ->
	not truthifying_value(Literal,Formula).

%%
%%	Returns a #variable_assignment which is the same as the one provided but
%%	with the opposite value i.e. true becomes false and false becomes true.
%%		Variable_Assignment: The #variable_assignment to be negated
%%			#variable_assignment
%%
negate_variable_assignment(Variable_Assignment) ->
    erlang:setelement(#variable_assignment.value,Variable_Assignment,not Variable_Assignment#variable_assignment.value).
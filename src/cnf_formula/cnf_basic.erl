%% Author: Francis Stephens
%% Created: 25 Feb 2008
%% Description: TODO: Add desciption to cnf_formula
-module(cnf_basic).

%%
%%	Include files
%%

-include("sat_records.hrl").
-include("cnf_formula_records.hrl").

%%
%%	Exported Functions
%%

%%	Creational functions
-export([def_2_formula/1]).
%%	Enquiry functions
%% TODO this module does not export a get_literal_counts/1 function
-export([get_var_vals/1,get_variable/2,get_variables/1,get_literal_negation/2,get_clauses/1,get_literals/2,get_variable_value/2,eval_literal/2,get_formula_satisfiable/1]).
%%	Modifying functions
-export([assign_variable/2,assign_variables/2]).

%%
%% API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Creational Functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a formula made from a formula_def where every variable is assigned indetermined.
%%		Formula_Def
%%			#formula_def
%%
def_2_formula({formula_def,Clause_Defs}) ->
    Clauses = lists:map(fun formula_util:clause_def_2_clause/1,Clause_Defs),
	#formula{variable_dict=formula_util:make_v_dict(Clauses),clauses=Clauses}.


%%******************************%%
%%	Local Creational functions	%%
%%******************************%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Enquiry Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a list of pairs of variables and ternary values [{Atom(),true|false|indetermined}]
%%		Formula
%%			#formula
%%
get_var_vals({formula,Variable_Dict,_Clauses}) ->
	dict:to_list(Variable_Dict).

%%
%%	Returns a list of variables for the formula provided
%%		Formula
%%			#formula
%%
get_variables({function,Variable_Dict,_Clauses}) ->
	dict:fetch_keys(Variable_Dict).

%%
%%	Returns a list of clauses for the formula provided
%%		Formula
%%			#formula
%%
get_clauses({formula,_Variable_Dict,Clauses}) -> Clauses.

%%
%%	Returns either subsumed, unsubsumed or empty.
%%	
%%	A clause is empty iff every literal in it evaluates to false.
%%	A clause is subsumed iff there exists one literal which evaluates to true.
%%	A clause is unsubsumed iff it contains at least one literal not yet assigned.
%%	If a clause contains no literals a function clause exception will be thrown.
%%		
%%		Clause
%%			#clause
%%		Formula: The formula in which this clause appears
%%			#formula
%%
get_clause_subsumed({clause,Literals},Formula) -> 
	get_literals_subsumed(Literals,Formula).

get_literals_subsumed([],_Formula) ->
	empty;

get_literals_subsumed([Literal|Literals],Formula) ->
	case eval_literal(Literal,Formula) of
		indetermined
			->	unsubsumed;
		true
			->	subsumed;
		false
			->	get_literals_subsumed(Literals,Formula)
	end.

%%
%%	Returns a list of literals for the clause provided
%%		Clause
%%			#clause
%%		Formula: The formula in which the clause appears
%%			#formula
%%
get_literals({clause,Literals},_Formula) ->
	Literals.

%%
%%	Returns either true, false or indetermined depending on the value of the variable provided
%%	in the formula provided.
%%	
%%	Beware the potentially misleading name of this function.  This does not take into account the
%%	negation of a literal just the value of it's variable's assignment.
%%
%%	Will throw an exception if the variable is not found in the formula
%%		Variable.  The variable whose value we seek
%%			atom()
%%		Formula.  The formula in which we seek it
%%			#formula
%%
get_variable_value(Variable,{formula,Variable_Dict,_Clauses}) ->
	dict:fetch(Variable,Variable_Dict).

%%
%%	Returns either true, false or indetermined depending on the value of the variable
%%	used in the literal provided with respect to the formula provided.
%%		Literal.  The literal whose value we seek
%%			#literal
%%		Formula.  The formula in which we seek it
%%			#formula
%%
get_literal_value({literal,_Negation,L_Name},Formula) ->
	get_variable_value(L_Name,Formula).

%%
%%	Returns the name, string(), of this literal.
%%	The name of a literal is the name of its variable
%%		Literal
%%			#literal
%%		Formula, the formula in which this literal appears, here included only for consistency
%%			#formula
%%
get_variable({literal,_Negation,L_Name},_Formula) ->
	L_Name.

%%
%%	Returns the negation of this literal, which is either true or false
%%	In the propositional calc:
%%	for the variable p, negation=true means p and negation=false means ~p
%%		Literal
%%			#literal
%%		Formula, the formula in which this literal appears, here included only for consistency
%%			#formula
%%
get_literal_negation({literal,Negation,_L_Name},_Formula) ->
	Negation.

%%
%% 	Evaluates the literal provided
%%	A literal evaluates to true iff its negation is the same as it's value
%%	A literal evaluates to indetermined iff its value is indetermined
%%	Otherwise a literal evaluates to false
%%		Literal.  The literal to evaluate
%%			#literal
%%		Formula.  The formula from which this literal comes
%%			#formula
%%	
%%
eval_literal(Literal,Formula) ->
	Negation = get_literal_negation(Literal,Formula),
	Value = get_literal_value(Literal,Formula),
	ternary_logic:not3(ternary_logic:xor3(Negation,Value)).

%%
%%	Returns true (satisfied) false (not satisfiable) or indetermined (not determined)
%%	This assumes that the Formula provided is consistent and has the following properties:
%%
%%	A formula is satisfied (true) iff every clause is subsumed.
%%	A formula is unsatisfiable (false) iff at least one clause is empty.
%%	A formula is indetermined iff at least one clause is unsubsumed and none are empty
%%
get_formula_satisfiable(Formula={formula,_Variable_Dict,Clauses}) ->
	Fun = 	fun(Clause,unsubsumed)->
				case get_clause_subsumed(Clause,Formula) of
					empty
						->	empty;               
					_Un_Subsumed
						->	unsubsumed
            	end;
			(Clause,_Empty_Or_Subsumed) ->
				get_clause_subsumed(Clause,Formula)end,
	case util:foldl_abandon(Fun,nil,Clauses,fun(Atom)-> Atom /= empty end) of
		subsumed
			->	true;
		empty
			->	false;
		unsubsumed
			->	indetermined
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Modifying Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a formula identical to the one provided but with the variable assignment applied
%%		Formula
%%			#formula
%%		Variable_Assignment
%%			#variable_assignment
%%
assign_variable({variable_assignment,V_Name,Value},{formula,Variable_Dict,Clauses}) ->
	New_V_Dict = dict:store(V_Name,Value,Variable_Dict),
	#formula{variable_dict=New_V_Dict,clauses=Clauses}.

%%
%%	Returns a formula identical to the one provided but with each of the variable assignments applied
%%		Formula
%%			#formula
%%		Variable_Assignments
%%			[#variable_assignment]
%%
assign_variables(Assignments,Formula) ->
    lists:foldl(fun assign_variable/2,Formula,Assignments).

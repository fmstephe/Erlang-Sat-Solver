%% Author: Francis Stephens
%% Created: 17 Jan 2008
%% Description: TODO: Add description to sat_algorithms
-module(sat_algorithms).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("sat_macros.hrl").

%%
%% Exported Functions
%%
-export([is_formula_satisfiable/1,assign_variable/2,choose_variable_naive/1,unit_prop/1,unit_prop_single/1,null_metadata/1]).

%%
%% API Functions
%%

%%
%%	Returns a #sat_result indicating whether the formula contained 
%%	within the #sat_state provided is satisfied, unsatisfiable or indetermined.
%%	The #sat_state provided is returned in #sat_result unchanged
%%
%%		Sat_State
%%			#sat_state
%%
is_formula_satisfiable(Sat_State) ->
    #sat_result{satisfiable=?FORMULA_DATA_STRUCTURE:get_formula_satisfiable(Sat_State#sat_state.formula),sat_state=Sat_State}.

%%
%%	
%%
assign_variable(Variable_Assignment,Formula) ->
    ?FORMULA_DATA_STRUCTURE:assign_variable(Variable_Assignment,Formula).

%%
%%	Returns a variable assignment, #variable_assignment, consisting of a variable name and the value true.
%%	The variable is guaranteed to be unassigned for the formula provided
%%		Formula
%%			#formula
%%
choose_variable_naive(Sat_State) ->
	Var_Vals = ?FORMULA_DATA_STRUCTURE:get_var_vals(Sat_State#sat_state.formula),
	get_first_unassigned(Var_Vals).

%%
%%	Returns a variable assignment, #variable_assignment, which is the name of the first variable found with value indetermined
%%		Var_Vals. List of variable names and their assigned values
%%			[{atom(),true|false|indetermined}]
%%
get_first_unassigned([]) ->
    #variable_assignment{l_name=no_variable,value=true};
    
get_first_unassigned([{Name,indetermined}|_Var_Vals]) ->
	#variable_assignment{l_name=Name,value=true};

get_first_unassigned([_Var_Val|Var_Vals]) ->
	get_first_unassigned(Var_Vals).

%%
%%	Returns a #sat_result where where each unit literal occurring in the formula provided evaluates to true.
%%	
%%	This function is called recursively until no new unit literals are found.
%%	
%%	A unit literal is a literal occurring in an unsubsumed clause such that it is the only literal
%%	whose variable remains unassigned.  The only way to make that clause subsumed is to force the literal
%%	to evaluate to true.
%%		Formula:
%%			#formula
%%
unit_prop(Sat_State) ->
	case all_unit_literals(Sat_State#sat_state.formula) of
        []
			->	is_formula_satisfiable(Sat_State);
        Unit_Literals
        	->	test_and_set_ul(Unit_Literals,Sat_State)
    end.

%%
%%	Returns a #sat_result which is unsatisfiable iff the Unit_Literals contain complimentary
%%	(p and ~p) literals otherwise the unit literals in Unit_Literals are made true in Formula
%%	and unit_prop is called (tail)recursively
%%
%%	Unit_Literals:
%%		[#literal]
%%	Formula:
%%		#formula
%%
test_and_set_ul(Unit_Literals,Sat_State) ->
	case opposed_unit_lit(Unit_Literals,Sat_State#sat_state.formula) of
		true 
        	->	#sat_result{satisfiable=false,sat_state=Sat_State};
		false 
          	-> 	New_Formula = make_literals_true(Unit_Literals,Sat_State#sat_state.formula),
				New_State = #sat_state{formula=New_Formula,state_metadata=Sat_State#sat_state.state_metadata},
            	unit_prop(New_State)
	end.

%%
%%	Returns a #sat_result where where each unit literal occurring in the formula provided evaluates to true.
%%	
%%	This function is called only once and further unit literals may exist after the single call.
%%	
%%	A unit literal is a literal occurring in an unsubsumed clause such that it is the only literal
%%	whose variable remains unassigned.  The only way to make that clause subsumed is to force the literal
%%	to evaluate to true.
%%		Formula:
%%			#formula
%%
unit_prop_single(Sat_State) ->
	Unit_Literals = all_unit_literals(Sat_State#sat_state.formula),
	case opposed_unit_lit(Unit_Literals,Sat_State#sat_state.formula) of
		true
			->	#sat_result{satisfiable=false,sat_state=Sat_State};
		false
			->	New_Formula = make_literals_true(Unit_Literals,Sat_State#sat_state.formula),
				New_State = #sat_state{formula=New_Formula,state_metadata=Sat_State#sat_state.state_metadata},
				#sat_result{satisfiable=?FORMULA_DATA_STRUCTURE:get_formula_satisfiable(New_Formula),sat_state=New_State}
	end.

%%
%% Local Functions
%%

%%
%%	Returns a list of all unit literals occurring in the formula provided.
%%	A unit literal is a literal occurring in an unsubsumed clause such that it is the only literal
%%	whose variable remains unassigned.
%%		Formula: The formula from which to extract all unit literals
%%			#formula
%%
all_unit_literals(Formula) ->
	Clauses = ?FORMULA_DATA_STRUCTURE:get_clauses(Formula),
	Filter_Fun = fun(Clause)->test_unit_lit(Clause,Formula)end,
	Map_Fun = fun (C) -> lists:nth(1,?FORMULA_DATA_STRUCTURE:get_literals(C,Formula)) end,
	util:filter_map(Filter_Fun,Map_Fun,Clauses).

%%
%%	Determines, for a list of literals, whether there is exactly one literal
%%	which remains unassigned.
%%	Since assigned literals evaluating to false are removed from their clauses
%%	and literals evaluating to true result in their clause being removed only 
%%	unassigned literals ever appear in clauses and so a unit literal will appear as
%%	the only literal in its clause.
%%	If there exists one (unit) literal then it is returned
%%	Otherwise empty is returned.
%%	TODO this function relies on the fact that literals that have been assigned are removed, that's not a good assumption
%%
test_unit_lit(Clause,Formula) ->
	Literals = ?FORMULA_DATA_STRUCTURE:get_literals(Clause,Formula),
	case length(Literals) of
		1
			-> true;
		_More_Than_One
			-> false
	end.

%%
%%	Determines, for a given literal and list of literals, whether the negation of that literal
%%	occurs in the list.
%%
opposed_unit_lit([],_Formula) -> false;

opposed_unit_lit([Literal|Literals],Formula) ->
	case sat_util:has_negation(Literal,Literals,Formula) of
		true -> true;
		false -> opposed_unit_lit(Literals,Formula)
	end.

%%
%%	Assigns, in a formula, the values for the given set of literals.
%%	Each assignment makes a literal in the literal set evaluate to true.
%%
make_literals_true(Literals,Formula) ->
	Name_Values = [#variable_assignment{l_name=?FORMULA_DATA_STRUCTURE:get_variable(Literal,Formula),value=sat_util:truthifying_value(Literal,Formula)}|| Literal <- Literals],
	?FORMULA_DATA_STRUCTURE:assign_variables(Name_Values,Formula).

%%
%%	Returns a #sat_state with state_metadata set to null.
%%	This is a helper function for solvers which do not make use of state metadata.
%%
%%	Formula_Def: The formula definition to be converted and to which metadata is to be attached
%%		#formula_def
%%
null_metadata(Formula_Def) ->
	Formula = ?FORMULA_DATA_STRUCTURE:def_2_formula(Formula_Def),
	#sat_state{formula=Formula,state_metadata=null}.
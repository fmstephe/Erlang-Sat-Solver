%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description: TODO: Add description to davis_putnam
-module(two_variable_branch_dp).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("sat_macros.hrl").

%%
%% Exported Functions
%%
-export([get_functions/0]).

-compile(export_all).

%%
%% API Functions
%%

%%
%% Local Functions
%%

%%
%%	Returns the function which define a Davis-Putnam SAT solver as a #dp_funs.
%%
get_functions() ->
	Pre_Funs = [fun sat_algorithms:unit_prop/1],
    Branch_Fun = fun branch_fun/1,
	Post_Funs = [],
	Init_Sat_State = fun sat_algorithms:null_metadata/1,
	#dp_funs{strategy_name=most_common_first,pre_funs=Pre_Funs,branch_fun=Branch_Fun,post_funs=Post_Funs,init_sat_state=Init_Sat_State}.

%%
%%	Returns the Workstack provided with the top most #sat_state removed and replaced its expansion.  A #sat_state
%%	is expanded by selecting an unassigned variable in its formula and assigning it true and false in two new 
%%	#sat_states.
%%
%%	Workstack: List of #sat_states representing the search work currently available
%%		[#sat_state]
%%
branch_fun([Sat_State|Workstack]) ->
	Variable_Assignments = get_popular_variables(Sat_State),
    produce_new_states(Sat_State,Variable_Assignments,Workstack).

produce_new_states(Sat_State,{Variable_Assignment1,Variable_Assignment2},Workstack) 
  when Variable_Assignment2#variable_assignment.l_name =:= no_variable ->
    N_Variable_Ass1 = sat_util:negate_variable_assignment(Variable_Assignment2),
    Formula1 = ?FORMULA_DATA_STRUCTURE:assign_variable(Variable_Assignment1,Sat_State#sat_state.formula),
    Formula2 = ?FORMULA_DATA_STRUCTURE:assign_variables(N_Variable_Ass1,Sat_State#sat_state.formula),
    State1 = #sat_state{formula=Formula1,state_metadata=Sat_State#sat_state.state_metadata},
    State2 = #sat_state{formula=Formula2,state_metadata=Sat_State#sat_state.state_metadata},
	[State1,State2|Workstack];

produce_new_states(Sat_State,{Variable_Assignment1,Variable_Assignment2},Workstack) ->
    N_Variable_Ass1 = sat_util:negate_variable_assignment(Variable_Assignment1),
    N_Variable_Ass2 = sat_util:negate_variable_assignment(Variable_Assignment2),
    Formula1 = ?FORMULA_DATA_STRUCTURE:assign_variables([Variable_Assignment1,Variable_Assignment2],Sat_State#sat_state.formula),
    Formula2 = ?FORMULA_DATA_STRUCTURE:assign_variables([Variable_Assignment1,N_Variable_Ass2],Sat_State#sat_state.formula),
    Formula3 = ?FORMULA_DATA_STRUCTURE:assign_variables([N_Variable_Ass1,Variable_Assignment2],Sat_State#sat_state.formula),
    Formula4 = ?FORMULA_DATA_STRUCTURE:assign_variables([N_Variable_Ass1,N_Variable_Ass2],Sat_State#sat_state.formula),
    State1 = #sat_state{formula=Formula1,state_metadata=Sat_State#sat_state.state_metadata},
    State2 = #sat_state{formula=Formula2,state_metadata=Sat_State#sat_state.state_metadata},
    State3 = #sat_state{formula=Formula3,state_metadata=Sat_State#sat_state.state_metadata},
	State4 = #sat_state{formula=Formula4,state_metadata=Sat_State#sat_state.state_metadata},
	[State1,State2,State3,State4|Workstack].

%%
%%	Returns a 
%%
get_popular_variables(Sat_State) ->
	Formula = Sat_State#sat_state.formula,
	Clauses = ?FORMULA_DATA_STRUCTURE:get_clauses(Formula),
	Literal_Count = count_literals(Clauses,Formula,dict:new()),
	Choose_Variable = fun(New_Variable,Variables={Variable1,Count1,_Variable2,Count2}) ->
						New_Count = dict:fetch(New_Variable,Literal_Count),
						case New_Count > Count1 of
							true
								->	{New_Variable,New_Count,Variable1,Count1};
							false
								->	case New_Count > Count2 of
                                        true
                                        	->	{Variable1,Count1,New_Variable,New_Count};
                                    	false
                                        	->	Variables
                                    end
						end
					 end,
	{Variable1,_Count1,Variable2,_Count2} = lists:foldl(Choose_Variable,{no_variable,-1,no_variable,-2},dict:fetch_keys(Literal_Count)),
	{#variable_assignment{l_name=Variable1,value=true},#variable_assignment{l_name=Variable2,value=true}}.

count_literals([],_Formula,Counting_Dict) ->
    Counting_Dict;
  
count_literals([Clause|Clauses],Formula,Counting_Dict) ->
	Literals = ?FORMULA_DATA_STRUCTURE:get_literals(Clause,Formula),
	Get_Variable = 	fun(Literal) ->
						?FORMULA_DATA_STRUCTURE:get_variable(Literal,Formula)
					end,
	Variables = lists:map(Get_Variable,Literals),
	Literal_Counter = fun(Variable,Dict) ->
						case ?FORMULA_DATA_STRUCTURE:get_variable_value(Variable,Formula) of
							indetermined
								->	dict:update_counter(Variable,1,Dict);
							_True_Or_False
								->	Dict
						end
					  end,
	New_Dict = lists:foldl(Literal_Counter,Counting_Dict,Variables),
	count_literals(Clauses,Formula,New_Dict).
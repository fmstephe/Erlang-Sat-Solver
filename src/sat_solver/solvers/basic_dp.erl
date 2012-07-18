%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description: TODO: Add description to davis_putnam
-module(basic_dp).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("sat_macros.hrl").

%%
%% Exported Functions
%%
-export([get_functions/0]).

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
	Pre_Funs = [fun sat_algorithms:is_formula_satisfiable/1],
    Branch_Fun = fun branch_fun/1,
	Post_Funs = [],
	Init_Sat_State = fun sat_algorithms:null_metadata/1,
	#dp_funs{strategy_name=dp_basic,pre_funs=Pre_Funs,branch_fun=Branch_Fun,post_funs=Post_Funs,init_sat_state=Init_Sat_State}.

%%
%%	Returns the Workstack provided with the top most #sat_state removed and replaced its expansion.  A #sat_state
%%	is expanded by selecting an unassigned variable in its formula and assigning it true and false in two new 
%%	#sat_states.
%%
%%	Workstack: List of #sat_states representing the search work currently available
%%		[#sat_state]
%%
branch_fun([Sat_State|Workstack]) ->
	Variable_Assignment = {variable_assignment,L_Name,L_Value} = sat_algorithms:choose_variable_naive(Sat_State),
    Left_Formula = sat_algorithms:assign_variable(Variable_Assignment,Sat_State#sat_state.formula),
	Left_State = #sat_state{formula=Left_Formula,state_metadata=Sat_State#sat_state.state_metadata},
	Right_Formula = sat_algorithms:assign_variable({variable_assignment,L_Name,not L_Value},Sat_State#sat_state.formula),
	Right_State = #sat_state{formula=Right_Formula,state_metadata=Sat_State#sat_state.state_metadata},
	[Left_State,Right_State|Workstack].
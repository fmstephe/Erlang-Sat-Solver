%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description: TODO: Add description to davis_putnam
-module(n_variable_branch_dp).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("sat_macros.hrl").

%%
%%
%%

-define(SET_LIMIT,2).

-define(CONST,const).
-define(NON_CONST,non_const).

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
	Assignments = get_popular_variables(Sat_State),
    Assignment_Combinations = util:combinate(Assignments),
    produce_new_states(Sat_State,Assignment_Combinations,Workstack).

%%
%%	Returns a new workstack which contains Workstack with all of the assignment combinations
%%	applied to the formula in Sat_State and pushed on top in the order they appear in 
%%	Assignments_Comb.
%%
%%	Sat_State: Contains the formula to which the many assignments will be made.
%%		#sat_state
%%	Assignments_Comb: List of assignments to be made to the formula in Sat_State
%%		[[#variable_assignment]]
%%	Workstack: The existing workstack onto which new work will be pushed
%%		[#sat_state]
%%
produce_new_states(_Sat_State,[],Workstack) ->
    Workstack;
    
produce_new_states(Sat_State,[Assignments|Assignments_Comb],Workstack) ->
    New_Formula = ?FORMULA_DATA_STRUCTURE:assign_variables(Assignments,Sat_State#sat_state.formula),
    New_State = #sat_state{formula=New_Formula,state_metadata=Sat_State#sat_state.state_metadata},
    [New_State|produce_new_states(Sat_State,Assignments_Comb,Workstack)].

%%
%%	Returns a 
%%
get_popular_variables(Sat_State) ->
	Formula = Sat_State#sat_state.formula,
	Clauses = ?FORMULA_DATA_STRUCTURE:get_clauses(Formula),
	Literal_Count = count_literals(Clauses,Formula,dict:new()),
	Choose_Variable = fun(Variable,Variable_Counts) ->
						Count = dict:fetch(Variable,Literal_Count),
						case length(Variable_Counts) < ?SET_LIMIT of
                            true
                            	->	insert_variable({Variable,Count},Variable_Counts,?NON_CONST);
                            false
                            	->	insert_variable({Variable,Count},Variable_Counts,?CONST)
                        end
					 end,
	Variable_Counts = lists:foldl(Choose_Variable,[],dict:fetch_keys(Literal_Count)),
    [[#variable_assignment{l_name=Variable,value=true},#variable_assignment{l_name=Variable,value=false}]||
     {Variable,_Count}<-Variable_Counts].

%%
%%	Returns a list of {Atom(),Integer()}.  Where the {Atom(),Integer()} provided as Variable_Count
%%	has been inserted into the list preserving the descending ordering of the list w.r.t. the integer
%%	element.  The list will not grow any larger if Const_Or_Not is const.
%%
%%	If Const_Or_Not is 'const' and Variable_Count's integer is less than all of those already in the list
%%	then Variable_Count will not be added to the list, and the list will remain  unchanged.  Otherwise it
%%	will be inserted at the appropriate point in the list and the least (and last) member of the list will
%%	be removed.
%%
%%	If Const_Or_Not is 'non_const' then Variable_Count will be added to Variable_Counts at the appropriate
%%	spot, preserving order.
%%
%%	Variable_Count: A record of a variable and the number of clauses it appears in
%%		{Atom(),Integer()}
%%	Variable_Counts: A list of such records ordered by the integer member
%%		[{Atom(),Integer()}]
%%	Const_Or_Not: Indicates whether or not the list returned has the same length as Variable_Counts
%%		const | non_const
%%
insert_variable(_Variable_Count,[],?CONST) ->
    [];

insert_variable(Variable_Count,[],?NON_CONST) ->
    [Variable_Count];

insert_variable(Variable_Count={_Variable,Count},[{_Other_Variable,Other_Count}|[]],?CONST)
  when Count >= Other_Count ->
    [Variable_Count];

insert_variable(Variable_Count={_Variable,Count},[Other_Variable_Count={_Other_Variable,Other_Count}|[]],?NON_CONST)
  when Count >= Other_Count ->
    [Variable_Count,Other_Variable_Count];
    
insert_variable(Variable_Count={_Variable,Count},[Other_Variable_Count={_Other_Variable,Other_Count}|Variable_Counts],?CONST)
  when Count >= Other_Count  ->
    [Variable_Count,Other_Variable_Count|util:drop_last(Variable_Counts)];

insert_variable(Variable_Count={_Variable,Count},[Other_Variable_Count={_Other_Variable,Other_Count}|Variable_Counts],?NON_CONST)
  when Count >= Other_Count  ->
    [Variable_Count,Other_Variable_Count|Variable_Counts];

insert_variable(Variable_Count,[Other_Variable_Count|Variable_Counts],Const_Or_Not) ->
    [Other_Variable_Count|insert_variable(Variable_Count,Variable_Counts,Const_Or_Not)].

%%
%%
%%
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
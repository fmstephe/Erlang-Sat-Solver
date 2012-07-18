%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description: TODO: Add description to davis_putnam
-module(cnf_graph_dp).

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
	Pre_Funs = [fun is_satisfiable/1],
    Branch_Fun = fun branch_fun/1,
	Post_Funs = [],
	Init_Sat_State = fun graph_null_metadata/1,
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
	Variable_Assignment = {variable_assignment,L_Name,L_Value} = get_popular_literal(Sat_State),
    Left_Formula = assign_variable(Variable_Assignment,Sat_State#sat_state.formula),
	Left_State = #sat_state{formula=Left_Formula,state_metadata=Sat_State#sat_state.state_metadata},
	Right_Formula = assign_variable({variable_assignment,L_Name,not L_Value},Sat_State#sat_state.formula),
	Right_State = #sat_state{formula=Right_Formula,state_metadata=Sat_State#sat_state.state_metadata},
	[Left_State,Right_State|Workstack].

is_satisfiable(Sat_State) ->
    Formula = Sat_State#sat_state.formula,
    Satisfiable = ?FORMULA_GRAPH_STRUCTURE:get_formula_satisfiable(Formula),
    #sat_result{satisfiable=Satisfiable,sat_state=Sat_State}.

assign_variable(Variable_Assignment,Formula) ->
    ?FORMULA_GRAPH_STRUCTURE:assign_variable(Variable_Assignment,Formula).

get_popular_literal(Sat_State) ->
	Formula = Sat_State#sat_state.formula,
    Literal_Counts = ?FORMULA_GRAPH_STRUCTURE:get_literal_counts(Formula),
    Find_Biggest_Count = fun(Literal_Count,Acc_Count) ->
                                 Max = lists:max([	P_Count = Literal_Count#literal_count.positive_count,
                                 					N_Count = Literal_Count#literal_count.negative_count,
                                 					Acc_Count#literal_count.positive_count,
                         		 					Acc_Count#literal_count.negative_count]),
                                 if
                                     (Max =:= P_Count) or (Max =:= N_Count)
                                     	->	Literal_Count;
                                     true
                                     	->	Acc_Count
                                 end
                         end,
    Literal_Count = lists:foldl(Find_Biggest_Count,#literal_count{variable_name=null_variable,positive_count=-1,negative_count=-1},Literal_Counts),
    Variable_Name = Literal_Count#literal_count.variable_name,
    Positive_Count = Literal_Count#literal_count.positive_count,
    Negative_Count= Literal_Count#literal_count.negative_count,
    io:format("~n~nThe Assignment = ~p~n~n",[#variable_assignment{l_name=Variable_Name,value=Positive_Count > Negative_Count}]),
    #variable_assignment{l_name=Variable_Name,value=Positive_Count > Negative_Count}.

get_popular_variable(Sat_State) ->
	Formula = Sat_State#sat_state.formula,
    Literal_Counts = ?FORMULA_GRAPH_STRUCTURE:get_literal_counts(Formula),
    Find_Biggest_Count = fun(Literal_Count,Acc_Count) ->
                                 Max = lists:max([	P_Count = Literal_Count#literal_count.positive_count,
                                 					N_Count = Literal_Count#literal_count.negative_count,
                                 					Acc_Count#literal_count.positive_count,
                         		 					Acc_Count#literal_count.negative_count]),
                                 if
                                     (Max =:= P_Count) or (Max =:= N_Count)
                                     	->	Literal_Count;
                                     true
                                     	->	Acc_Count
                                 end
                         end,
    Literal_Count = lists:foldl(Find_Biggest_Count,#literal_count{variable_name=null_variable,positive_count=-1,negative_count=-1},Literal_Counts),
    Variable_Name = Literal_Count#literal_count.variable_name,
    Positive_Count = Literal_Count#literal_count.positive_count,
    Negative_Count= Literal_Count#literal_count.negative_count,
    io:format("~n~nThe Assignment = ~p~n~n",[#variable_assignment{l_name=Variable_Name,value=Positive_Count > Negative_Count}]),
    #variable_assignment{l_name=Variable_Name,value=Positive_Count > Negative_Count}.

%%
%%	Returns a #sat_state with state_metadata set to null where the formula data-structure is taken from cnf_graph.
%%
%%	Formula_Def: The formula definition to be converted and to which metadata is to be attached
%%		#formula_def
%%
graph_null_metadata(Formula_Def) ->
	Formula = ?FORMULA_GRAPH_STRUCTURE:def_2_formula(Formula_Def),
    Debug_Formula = ?FORMULA_GRAPH_STRUCTURE:assign_variable({variable_assignment,"X1",true},Formula),
    Debug_Formula2 = erlang:setelement(4,Debug_Formula,indetermined),
	#sat_state{formula=Debug_Formula2,state_metadata=null}.
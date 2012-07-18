%% Author: Francis Stephens
%% Created: 24 Nov 2008
%% Description: TODO: Add description to solve_and_measure
-module(solve_and_measure).

%%
%% Exported Functions
%%
-export([test_run/0,run/3]).

%%
%% Include files
%%

-include("sat_macros.hrl").
-include("sat_records.hrl").
-include("process_records.hrl").

%%
%% API Functions
%%

%%
%%	convenience method for quickly running the flavour of the day
%%
test_run()->
    solve(sat_test:sat4()),
    measure_and_print().

run(Var_Num_List,C_Num_List,Formula_Num) ->
    Args_Comb = [{Var_Num,C_Num,3}||Var_Num<-Var_Num_List,C_Num<-C_Num_List],
	Solve_And_Measure = fun(Args) ->
                                io:format("~n~nArgs = ~p~n~n",[Args]),
							util:do_n_times(fun()->	generate_and_solve(Args),
													measure_and_print(),
													tree_persist:clear_trees() end,
										   Formula_Num)
						end,
	lists:foreach(Solve_And_Measure,Args_Comb).

%%
%% TODO: Add description of generate_and_solve/function_arity
%%
generate_and_solve({Var_Num,C_Num,C_Size}) ->
    Generated_Formulae = generate(Var_Num,C_Num,C_Size),
    solve(Generated_Formulae).

generate(Var_Num,C_Num,C_Size) ->
	formula_generator:generate_formula(Var_Num,C_Num,C_Size).

solve(Generated_Formula) ->
	Funs = most_common_first_dp:get_functions(),
    dp_engine_tree_agg:is_satisfiable(Funs,Generated_Formula).

measure_and_print() ->
	Tree_Roots = tree_persist:get_all_tree_roots(),
	Measure_Print_Fun = fun(Tree_Root) ->
		Tree_Id = Tree_Root#tree_root.tree_id,
		Tree_Folder = test_util:get_file_path() ++ "/test" ++ util:int_to_string(Tree_Id),
		Root_Metric = root_to_solution_metrics:compute_metric(Tree_Id),
		metric_util:print_to_file(Root_Metric,Tree_Folder),
		Tree_Metadata = Tree_Root#tree_root.tree_metadata,
		tree_metadata_util:print_to_file(Tree_Metadata,Tree_Folder)
	end,
	lists:foreach(Measure_Print_Fun,Tree_Roots).

%%
%% Local Functions
%%

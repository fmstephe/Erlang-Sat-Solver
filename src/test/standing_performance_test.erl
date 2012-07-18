%% Author: Francis Stephens
%% Created: 8 Jun 2009
%% Description: TODO: Add description to standing_performance_test
-module(standing_performance_test).

%%
%%	Macros
%%
-define(LOGGED_TESTS,"C:\Sat_Test\Standing_Performance_Tests").
-define(STANDING_FORMULAE,?LOGGED_TESTS++"\Standing_Formulae").

-define(SMALL_STANDING_FORMULAE,"25-107-3-10.txt").
-define(MEDIUM_STANDING_FORMULAE,"50-214-3-10.txt").
-define(LARGE_STANDING_FORMULAE,"100-428-3-10.txt").

%%
%% Include files
%%

-include("sat_macros.hrl").
-include("sat_records.hrl").
-include("process_tree_records.hrl").
-include("dp_engine_concurrent_records.hrl").

%%
%% Exported Functions
%%
-export([run/0]).

%%
%% API Functions
%%

%%
%% Using the standing formulae files a set of performance benchmarks are created for todays date-time.
%%
run() ->
    File_Names = [?SMALL_STANDING_FORMULAE,?MEDIUM_STANDING_FORMULAE,?LARGE_STANDING_FORMULAE],
    Time_Stamp = util:get_filename_time_stamp(),
    lists:foreach(fun(File_Name) -> log_performance(File_Name,Time_Stamp) end,File_Names).

%%
%% Local Functions
%%

%%
%%	Logs the peformance, against a collection of standing formulae, for each of the solvers
%%	currently of interest.
%%
%%	Standing_Formulae_Name: The name of the formulae file for this benchmark
%%		String()
%%	Time_Stamp: The current date-time uniquely identifying when this benchmark was run
%%		String()
%%
log_performance(Standing_Formulae_Name,Time_Stamp) ->
    log_iterative_performance(Standing_Formulae_Name,Time_Stamp),
    log_concurrent_tree_performance(Standing_Formulae_Name,Time_Stamp),
    log_concurrent_list_performance(Standing_Formulae_Name,Time_Stamp).

%%
%%	Logs the performance of the iterative solver using the most_common_first strategy
%%
%%	Standing_Formulae_Name: The name of the formulae file for this benchmark
%%		String()
%%	Time_Stamp: The current date-time uniquely identifying when this benchmark was run
%%		String()
%%
log_iterative_performance(Standing_Formulae_Name,Time_Stamp) ->
    Dp_Funs = most_common_first_dp:get_functions(),
    run_and_log(Standing_Formulae_Name,"Concurrent_List",Time_Stamp,Dp_Funs,dp_engine_iterative).

%%
%%	Logs the performance of the concurrent solver using the most_common_first strategy and 
%%	employing a process tree for distributing work amongst processes.
%%
%%	Standing_Formulae_Name: The name of the formulae file for this benchmark
%%		String()
%%	Time_Stamp: The current date-time uniquely identifying when this benchmark was run
%%		String()
%%
log_concurrent_tree_performance(Standing_Formulae_Name,Time_Stamp) ->
    Dp_Funs = most_common_first_dp:get_functions(),
    Process_Tree_Args = #engine_concurrent_args{network_config=process_list_util:get_default_config(),comm_freq=50,process_tree_mod=process_tree},
    Dp_Funs_PT = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Process_Tree_Args),
	run_and_log(Standing_Formulae_Name,"Concurrent_Tree",Time_Stamp,Dp_Funs_PT,dp_engine_concurrent).

%%
%%	Logs the performance of the concurrent solver using the most_common_first strategy and 
%%	employing a process list for distributing work amongst processes.
%%
%%	Standing_Formulae_Name: The name of the formulae file for this benchmark
%%		String()
%%	Time_Stamp: The current date-time uniquely identifying when this benchmark was run
%%		String()
%%
log_concurrent_list_performance(Standing_Formulae_Name,Time_Stamp) ->
    Dp_Funs = most_common_first_dp:get_functions(),
    Process_Tree_Args = #engine_concurrent_args{network_config=process_list_util:get_default_config(),comm_freq=50,process_tree_mod=process_list},
    Dp_Funs_PL = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Process_Tree_Args),
    run_and_log(Standing_Formulae_Name,"Concurrent_List",Time_Stamp,Dp_Funs_PL,dp_engine_concurrent).

%%
%%	Logs the performance of the given solver using the provided strategy.
%%
%%	Standing_Formulae_Name: The name of the formulae file for this benchmark
%%		String()
%%	Solver_Strategy: A name which identifies this solver/strategy mix from the other being run on the same timestamp
%%		String()
%%	Time_Stamp: The current date-time uniquely identifying when this benchmark was run
%%		String()
%%	Dp_Funs: Defines both the solver strategy used and the work sharing mechanism, if one is needed
%%		#dp_funs
%%	Dp_Engine: The module name for a solver engine to use
%%		Atom()
%%
run_and_log(Standing_Formulae_Name,Solver_Strategy,Time_Stamp,Dp_Funs,Dp_Engine) ->
    Formulae = formula_util:consult_formulae_file(Standing_Formulae_Name,?STANDING_FORMULAE),
    Time_Solver = 	fun(Formula) ->
                  		{Time,_Sat_Result} = test_util:solve_and_time(Dp_Funs,Formula,Dp_Engine),
                  		Time
          			end,
	Times = lists:map(Time_Solver,Formulae),
    Mean_Time = stats_util:mean(Times),
    Standard_Deviation = stats_util:standard_deviation(Times),
    Total_Time = lists:foldl(fun(X,Y)-> X+Y end,0,Times),
    IO_Device = util:make_file(Standing_Formulae_Name,?LOGGED_TESTS++"/"++Time_Stamp++"/"++Solver_Strategy),
    io:format(IO_Device,"~n~nTimes = ~p~n~nTotal Time = ~p~n~nMean Time = ~p~n~nStandard Deviation~p",[Times,Total_Time,Mean_Time,Standard_Deviation]),
    file:close(IO_Device).
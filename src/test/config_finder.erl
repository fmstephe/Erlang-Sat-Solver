%% Author: Francis Stephens
%% Created: 18 Feb 2009
%% Description: TODO: Add description to config_finder
-module(config_finder).

%%
%% Include files
%%
-include("sat_records.hrl").
-include("dp_engine_concurrent_records.hrl").
-include("process_tree_records.hrl").

%%
%% Exported Functions
%%
-export([run/5]).

-compile(export_all).

%%
%%	Macros
%%
-define(MAX_TREE_SIZE,2).
-define(PROCESS_SPAWN_THRESHOLD,3).
-define(WORK_SHARING_THRESHOLD,4).
-define(WORK_SPLITTING_FUN,5).
-define(COMM_FREQUENCY,6).
-define(MAX_CHOICE,6).
-define(MUTATION_SMALL_LIMIT,15).
-define(MUTATION_LARGE_LIMIT,60).

%%
%% API Functions
%%

run(Var_Num,C_Num,Formula_Num,Run_Num,Generation_Num) ->
    Dp_Mod = most_common_first_dp,
    util:loop_n_times(fun(Engine_Args)->evolve(Engine_Args,Dp_Mod,Var_Num,C_Num,Formula_Num,Run_Num)end,get_initial_args(),Generation_Num).

evolve(Engine_Args,Dp_Mod,Var_Num,C_Num,Formula_Num,Run_Num) ->
    Engine_Args_List = mutate_engine_args(Engine_Args),
    Test_Configs = fun(Generated_Formula) ->
                           run_with_config(Generated_Formula,Engine_Args_List,Run_Num,Dp_Mod)
                   end,
    Merge_Dicts = fun(Dict1,Dict2) ->
                          dict:merge(fun(_Key,Val1,Val2)->Val1++Val2 end,Dict1,Dict2)
                  end,
    Generated_Formulae = formula_generator:generate_formulae(Var_Num,C_Num,3,Formula_Num),
    Runs = lists:map(Test_Configs,Generated_Formulae),
    Merged_Runs = lists:foldl(Merge_Dicts,dict:new(),Runs),
    get_best_config(Merged_Runs).

get_best_config(Runs_Dict) ->
    Engine_Args_List = dict:fetch_keys(Runs_Dict),
    Make_Mean = fun(Engine_Args,Mean_Dict) ->
                        Run_Mean = stats_util:mean(dict:fetch(Engine_Args,Runs_Dict)),
                        dict:store(Engine_Args,Run_Mean,Mean_Dict)
                end,
    Mean_Runs_Dict = lists:foldl(Make_Mean,dict:new(),Engine_Args_List),
    io:format("~n~nRuns_Dict = ~p~n~n~n~nMean_Runs_Dict = ~p~n~n",[dict:to_list(Runs_Dict),dict:to_list(Mean_Runs_Dict)]),
    Get_Best = fun(Engine_Args,Best_Engine_Args) ->
    					Best_Time = dict:fetch(Best_Engine_Args,Mean_Runs_Dict),
						New_Time = dict:fetch(Engine_Args,Mean_Runs_Dict),
						case Best_Time < New_Time of
							true
								->	Best_Engine_Args;
							false
								->	Engine_Args
						end
				end,
    lists:foldl(Get_Best,hd(Engine_Args_List),Engine_Args_List).

get_initial_args() ->
    Work_Splitting_Fun = fun(List)-> util:split_alternate(List) end,
    Network_Config = #network_config{max_tree_size=20,process_spawn_threshold=3,work_sharing_threshold=34,work_splitting_fun=Work_Splitting_Fun},
    #engine_concurrent_args{network_config=Network_Config,comm_freq=73,process_tree_mod=process_tree_light}.

mutate_engine_args(Engine_Args) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    Mutation_Index = random:uniform(?MAX_CHOICE-1)+1,
    Network_Config = Engine_Args#engine_concurrent_args.network_config,
    case Mutation_Index of
        Integer when Integer =:= ?MAX_TREE_SIZE; Integer =:= ?PROCESS_SPAWN_THRESHOLD; Integer =:= ?WORK_SHARING_THRESHOLD
        	->	Mutation_Amount = random:uniform(?MUTATION_SMALL_LIMIT),
                {Plus_Value,Minus_Value} = get_legal_values(erlang:element(Mutation_Index,Network_Config),Mutation_Amount),
            	Network_Config_Plus = erlang:setelement(Mutation_Index,Network_Config,Plus_Value),
                Network_Config_Minus = erlang:setelement(Mutation_Index,Network_Config,Minus_Value),
                [setelement(#engine_concurrent_args.network_config,Engine_Args,Network_Config_Plus),
                 setelement(#engine_concurrent_args.network_config,Engine_Args,Network_Config_Minus),
                 Engine_Args];
		?WORK_SPLITTING_FUN
			->	Work_Splitter_1 = fun(List)->util:split_alternate(List) end,
				Work_Splitter_2 = fun(List)->util:split_in_half(List) end,
				Network_Config_1 = erlang:setelement(Mutation_Index,Network_Config,Work_Splitter_1),
                Network_Config_2 = erlang:setelement(Mutation_Index,Network_Config,Work_Splitter_2),
				[setelement(#engine_concurrent_args.network_config,Engine_Args,Network_Config_1),
                 setelement(#engine_concurrent_args.network_config,Engine_Args,Network_Config_2)];
        ?COMM_FREQUENCY
            ->	Mutation_Amount = random:uniform(?MUTATION_LARGE_LIMIT),
                {Plus_Value,Minus_Value} = get_legal_values(erlang:element(#engine_concurrent_args.comm_freq,Engine_Args),Mutation_Amount),
                [setelement(#engine_concurrent_args.comm_freq,Engine_Args,Plus_Value),
                 setelement(#engine_concurrent_args.comm_freq,Engine_Args,Minus_Value),
                 Engine_Args]
    end.

get_legal_values(Value,Modification) ->
    Plus_Value = Value+Modification,
    Minus_Value = Value-Modification,
    if
    	Minus_Value =< 0
			->	{Plus_Value,1};
		true
			->	{Plus_Value,Minus_Value}
	end.

%%
%%	Returns a dict() mapping each engine_arg config to a list of timings
%%
run_with_config(Generated_Formula,Engine_Args_List,Run_Num,Dp_Mod) ->
    Dp_Funs = Dp_Mod:get_functions(),
    Dp_Funs_List = [setelement(#dp_funs.engine_args,Dp_Funs,Engine_Args)||Engine_Args<-Engine_Args_List],
	Solve = fun(Dp_Funs_Arg) ->
				{Time,_Sat_Result} = test_util:solve_and_time(Dp_Funs_Arg,Generated_Formula,dp_engine_concurrent),
                Time
			end,
    Solve_For_Args = fun(Dp_Funs_Arg) ->
                             {Dp_Funs_Arg#dp_funs.engine_args,util:generate_list(fun()->Solve(Dp_Funs_Arg)end,Run_Num)}
                     end,
    Runs = lists:map(Solve_For_Args,Dp_Funs_List),
    Fold_Runs = fun({Engine_Args,Timings},Dict) ->
                        dict:append_list(Engine_Args,Timings,Dict)
                end,
    lists:foldl(Fold_Runs,dict:new(),Runs).

%%
%% Local Functions
%%
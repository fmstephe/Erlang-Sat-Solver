%% Author: Francis Stephens
%% Created: 26 Feb 2009
%% Description: TODO: Add description to config_exp
-module(config_exp).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("process_tree_records.hrl").
-include("dp_engine_concurrent_records.hrl").

%%
%%	Macros
%%
-define(MAX_TREE_SIZE,2).
-define(PROCESS_SPAWN_THRESHOLD,3).
-define(WORK_SHARING_THRESHOLD,4).
-define(WORK_SPLITTING_FUN,5).
-define(COMM_FREQUENCY,6).
-define(FORMULA_NUM,200).

%%
%% Exported Functions
%%
-export([
         exp_pst/0,exp_wst/0,sat_ratio/3,exp_mts/0,quickcheck/0,compare_configs/5
        ]).

%%
%% API Functions
%%

quickcheck() ->
    {ok,PST_File}= util:make_file("quick_check.txt",test_util:get_file_path()++"/experiments"),
    compare_configs(PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,4),100,428,200),
    compare_configs(PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,4,1),100,428,200),
    compare_configs(PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,6),100,428,200),
    compare_configs(PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,6,1),100,428,200),
    file:close(PST_File).

exp_pst() ->
    do_exp_pst("process_spawn_threshold_middle.txt",100,[428,429],?FORMULA_NUM).

do_exp_pst(FileName,Var_Num,C_Num,Formula_Num) ->
    {ok,PST_File}= util:make_file(FileName,test_util:get_file_path()++"/experiments"),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,10),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,10,1),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,9),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,9,1),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,8),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,8,1),Var_Num,C_Num,Formula_Num]),
	time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,7),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,7,1),Var_Num,C_Num,Formula_Num]),
	time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,6),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,6,1),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,5),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,5,1),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,4),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,4,1),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,3),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,3,1),Var_Num,C_Num,Formula_Num]),
	time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,2),Var_Num,C_Num,Formula_Num]),
    time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,2,1),Var_Num,C_Num,Formula_Num]),
	time_and_print([PST_File,dp_funs_pair(?PROCESS_SPAWN_THRESHOLD,1,1),Var_Num,C_Num,Formula_Num]),
    file:close(PST_File),
    ok.

exp_wst() ->
    do_exp_wst("work_sharing_threshold_middle.txt",100,[428,429],?FORMULA_NUM),
    do_exp_wst("work_sharing_threshold_unsat.txt",100,440,?FORMULA_NUM),
	do_exp_wst("work_sharing_threshold_really_unsat.txt",100,460,?FORMULA_NUM),
    do_exp_wst("work_sharing_threshold_sat.txt",100,410,?FORMULA_NUM),
    do_exp_wst("work_sharing_threshold_really_sat.txt",100,390,?FORMULA_NUM).

do_exp_wst(FileName,Var_Num,C_Num,Formula_Num) ->
    {ok,WST_File} = util:make_file(FileName,test_util:get_file_path()++"/experiments"),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,1,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,3,1),Var_Num,C_Num,Formula_Num),
	compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,5,1),Var_Num,C_Num,Formula_Num),
	compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,7,1),Var_Num,C_Num,Formula_Num),
	compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,9,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,11,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,13,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,15,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,17,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,19,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,20,1),Var_Num,C_Num,Formula_Num),
    compare_configs(WST_File,dp_funs_pair(?WORK_SHARING_THRESHOLD,100,1),Var_Num,C_Num,Formula_Num),
    file:close(WST_File),
    ok.

exp_mts() ->
    do_exp_mts_middle("max_tree_size_exp_compare.txt",150,[656,658],?FORMULA_NUM).

do_exp_mts(FileName,Var_Num,C_Num,Formula_Num) ->
    {ok,MTS_File}= util:make_file(FileName,test_util:get_file_path()++"/experiments"),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,41),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,42),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,43),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,44),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,45),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,46),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,47),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,48),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,49),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,50),Var_Num,C_Num,Formula_Num),
    file:close(MTS_File),
    ok.

do_exp_mts_rep(FileName,Var_Num,C_Num,Formula_Num) ->
    {ok,MTS_File}= util:make_file(FileName,test_util:get_file_path()++"/experiments"),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,40),Var_Num,C_Num,Formula_Num),
    file:close(MTS_File),
    ok.

do_exp_mts_middle(FileName,Var_Num,C_Num,Formula_Num) ->
    {ok,MTS_File}= util:make_file(FileName,test_util:get_file_path()++"/experiments"),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,1,2),Var_Num,C_Num,40),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,2,4),Var_Num,C_Num,40),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,4,8),Var_Num,C_Num,40),
	compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,8,16),Var_Num,C_Num,40),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,16,32),Var_Num,C_Num,40),
    compare_configs(MTS_File,dp_funs_pair(?MAX_TREE_SIZE,32,64),Var_Num,C_Num,40),
    file:close(MTS_File),
    ok.

dp_funs_pair(Index,Value1,Value2) ->
    Dp_Funs = most_common_first_dp:get_functions(),
    Default_Args = dp_engine_concurrent:get_default_args(),
    Engine_Args1 = mutate_engine_args(Index,Value1,Default_Args),
    Engine_Args2 = mutate_engine_args(Index,Value2,Default_Args),
    Dp_Funs1 = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Engine_Args1),
    Dp_Funs2 = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Engine_Args2),
    {Dp_Funs1,Dp_Funs2}.

mutate_engine_args(Index,Value,Engine_Args) ->
    Network_Config = Engine_Args#engine_concurrent_args.network_config,
    case Index of
        Integer when Integer =:= ?MAX_TREE_SIZE; Integer =:= ?PROCESS_SPAWN_THRESHOLD; Integer =:= ?WORK_SHARING_THRESHOLD
        	->	New_Network_Config = erlang:setelement(Index,Network_Config,Value),
                setelement(#engine_concurrent_args.network_config,Engine_Args,New_Network_Config);
		?WORK_SPLITTING_FUN
			->	New_Network_Config = erlang:setelement(Index,Network_Config,Value),
				setelement(#engine_concurrent_args.network_config,Engine_Args,New_Network_Config);
        ?COMM_FREQUENCY
            ->	setelement(#engine_concurrent_args.comm_freq,Engine_Args,Value)
    end.

%%
%% Local Functions
%%
    
compare_configs(IO_Device,{Dp_Funs1,Dp_Funs2},Var_Num,C_Num,Formula_Num) ->
   	io:format(IO_Device,"~n~nConfiguration 1 = ~w ~n~nConfiguration 2 = ~w ~n~n(variables= ~p and clauses= ~p)~n~n",[Dp_Funs1,Dp_Funs2,Var_Num,C_Num]),
    Generated_Formulae = formula_generator:generate_formulae(Var_Num,C_Num,3,Formula_Num),
    Raw_Data = compare_solvers:generate_and_compare(Generated_Formulae,[{"Config1 = ",Dp_Funs1,dp_engine_concurrent},{"Config2 = ",Dp_Funs2,dp_engine_concurrent}]),
	Ratios = make_ratio(Raw_Data),
    io:format(IO_Device,"~n~n~p~n~n",[Ratios]),
    [{Name,Ratio,undefined}] = make_ratio(make_totals(Raw_Data)),
    io:format(IO_Device,"~n~n~p~n~n",[{Name,Ratio}]).
  

time_and_print(Args) ->
    {Time,_Return} = timer:tc(config_exp,compare_configs,Args),
    io:format("~n~nConfig Experiment ran in ~p milliseconds~n~n",[util:format_int(Time)]).
    
%%
%%	Only works when comparing two solvers
%%

%%
%%	Ratio 100 / 430 (428)
%%
sat_ratio(Var_Num,C_Num,Formula_Num) ->
    Generated_Formulae = formula_generator:generate_formulae(Var_Num,C_Num,3,Formula_Num),
    Dp_Funs = most_common_first_dp:get_functions(),
    Raw_Data = compare_solvers:generate_and_compare(Generated_Formulae,[{"Config = ",Dp_Funs,dp_engine_concurrent}]),
	Count_Sat = fun([{_Name,_Time,Sat}],{Sat_Count,Unsat_Count}) ->
              			case Sat of
                        	true
                            	->	{Sat_Count+1,Unsat_Count};
                            false
                            	->	{Sat_Count,Unsat_Count+1}
                        end
                end,
    Print = fun(Arg) ->
                    io:format("~n~n~p~n~n",[Arg])
            end,
    lists:foreach(Print,Raw_Data),
	{Sat_Count,Unsat_Count}= lists:foldl(Count_Sat,{0,0},Raw_Data),
    io:format("~n~n~p~n~n",[{Sat_Count,Unsat_Count}]),
    Sat_Count/Unsat_Count.

make_ratio(Raw_Data) ->
    Calc_Ratio = fun([{_Name1,Time1,Sat},{_Name2,Time2,Sat}]) ->
                           {"Ratio = ",Time1/Time2,Sat}
                   end,
    lists:map(Calc_Ratio,Raw_Data).

make_totals(Raw_Data) ->
   Fold_Total = fun(Formula_Run,Totals) ->
                           Run_And_Totals = lists:zip(Formula_Run,Totals),
                           lists:map(fun({{Name,Time,_Sat1},{Name,Total,_Sat2}}) -> {Name,Time+Total,undefined} end,Run_And_Totals)
                   end,
   [lists:foldl(Fold_Total,lists:map(fun({Name,_Time,Sat})-> {Name,0,Sat} end,hd(Raw_Data)),Raw_Data)].
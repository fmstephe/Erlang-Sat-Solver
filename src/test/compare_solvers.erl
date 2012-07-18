%% Author: Francis Stephens
%% Created: 28 Jan 2009
%% Description: TODO: Add description to compare_solvers
-module(compare_solvers).

%%
%% Include files
%%

-include("sat_macros.hrl").
-include("sat_records.hrl").
-include("worksharing_records.hrl").
-include("process_tree_records.hrl").
-include("process_list_records.hrl").
-include("dp_engine_concurrent_records.hrl").
-include("distributed_list_records.hrl").

%%
%% Exported Functions
%%
-export([
         run/3,
		 run/2,
         compare_for_formulae/2,
         generate_formulae_file/1,
         profile_run_times/2
		]).

-compile(export_all).

%%
%%	Records
%%

%%
%%	Macros
%%
-define(TEST_FORMULAE_FILE_PATH,"C:/Sat_Test/formulae").
-define(TEST_FORMULAE_FILE_NAME,"formula_file.txt").

%%
%%	API Functions
%%
run(Var_Num_List,C_Num_List,Formula_Num) ->
	run(Var_Num_List,C_Num_List,Formula_Num,1,16).

run(Var_Num_List,C_Num_List,Formula_Num,Node_Num,Process_Num) ->
	%% Create all combinations of variable and clause counts
    Args_Comb = [{Var_Num,C_Num,3}||Var_Num<-Var_Num_List,C_Num<-C_Num_List],
	Solve_And_Measure = fun({Vnum,Cnum,Csize}) ->
                            Formulae = generate(Vnum,Cnum,Csize,Formula_Num),
							Node_Names = auto_generate_node_names(Node_Num),
							Distributed_Config = organise_distributed_configs(Node_Names,Process_Num),
							set_up_and_compare(Formulae,Node_Num,Process_Num,Distributed_Config)
						end,
	lists:foreach(Solve_And_Measure,Args_Comb).

%%
%%	Returns ok.
%%	Prints to the console a list of run times and a total time taken to solve a list of formulae.
%%	Automatically generates a list of node names
%%
run(Node_Num,Process_Num) when is_integer(Node_Num) ->
    Formulae = formula_util:consult_formulae_file(?TEST_FORMULAE_FILE_NAME,?TEST_FORMULAE_FILE_PATH),
	Node_Names = auto_generate_node_names(Node_Num),
	Distributed_Config = organise_distributed_configs(Node_Names,Process_Num),
	set_up_and_compare(Formulae,Node_Num,Process_Num,Distributed_Config);

run(Node_Names,Process_Num) when is_list(Node_Names)->
	Formulae = formula_util:consult_formulae_file(?TEST_FORMULAE_FILE_NAME,?TEST_FORMULAE_FILE_PATH),
	Distributed_Config = organise_distributed_configs(Node_Names,Process_Num),
	set_up_and_compare(Formulae,length(Node_Names),Process_Num,Distributed_Config).

set_up_and_compare(Formulae,Node_Num,Process_Num,Distributed_Config) ->
	%% Choose from our premuim range of Davis Putnam variations
    Dp_Funs = most_common_first_dp:get_functions(),
	No_Cp_Funs = no_cp_dp:get_functions(),
    Dp_Funs_2Var = two_variable_branch_dp:get_functions(),
    Dp_Funs_NVar = n_variable_branch_dp:get_functions(),
	%% Organise the Config for the distributed case
	%% Organise the #engine_concurrent_args for each of our three different concurrency modes
	List_Network_Config = #list_network_config{network_size=Process_Num,
                    	work_sharing_threshold=2,
                    	work_splitting_fun=fun(List)->util:split_alternate(List)end},
	Tree_Network_Config = #network_config{max_tree_size=Process_Num,
                    	process_spawn_threshold=1,
                    	work_sharing_threshold=5,
                    	work_splitting_fun=fun(List)->util:split_alternate(List)end},
	Process_List_Args = #engine_concurrent_args{network_config=List_Network_Config,comm_freq=50,process_tree_mod=process_list},
    Process_Tree_Args = #engine_concurrent_args{network_config=Tree_Network_Config,comm_freq=50,process_tree_mod=process_tree},
	Distributed_List_Args = #engine_concurrent_args{network_config=Distributed_Config,comm_freq=50,process_tree_mod=distributed_process_list},
    %% Finally set up a (nearly) complete array of all possible engine configurations
	Dp_Funs_Tree = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Process_Tree_Args),
    Dp_Funs_List = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Process_List_Args),
	Dp_Funs_Distributed = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Distributed_List_Args),
    Dp_Funs_Tree_2Var = erlang:setelement(#dp_funs.engine_args,Dp_Funs_2Var,Process_Tree_Args),
    Dp_Funs_List_2Var = erlang:setelement(#dp_funs.engine_args,Dp_Funs_2Var,Process_List_Args),
	Dp_Funs_Distributed_2Var = erlang:setelement(#dp_funs.engine_args,Dp_Funs_2Var,Distributed_List_Args),
    Dp_Funs_Tree_NVar = erlang:setelement(#dp_funs.engine_args,Dp_Funs_NVar,Process_Tree_Args),
    Dp_Funs_List_NVar = erlang:setelement(#dp_funs.engine_args,Dp_Funs_NVar,Process_List_Args),
	Dp_Funs_Distributed_NVar = erlang:setelement(#dp_funs.engine_args,Dp_Funs_NVar,Distributed_List_Args),
    Dp_Funs_Tree_No_Cp = erlang:setelement(#dp_funs.engine_args,No_Cp_Funs,Process_Tree_Args),
    Dp_Funs_List_No_Cp = erlang:setelement(#dp_funs.engine_args,No_Cp_Funs,Process_List_Args),
	Dp_Funs_Distributed_No_Cp = erlang:setelement(#dp_funs.engine_args,No_Cp_Funs,Distributed_List_Args),
	Results = compare_for_formulae(Formulae,[{"Distributed",Dp_Funs_Distributed_2Var,dp_engine_concurrent}]),
	io:format("~n~nNumber of Nodes = ~p~nNumber of Processes = ~p~nSize of Network = ~p~n~n",[Node_Num,Process_Num,Node_Num*Process_Num]),
	analyse_and_print_results(Results).

auto_generate_node_names(Node_Num) ->
	Node_Numbers = lists:seq(1, Node_Num),
	%% Little bit awkward but for your own machine you should change the string literal to @your-machine-name
	lists:map(fun (Num) -> Pre = util:int_to_string(Num), Name = Pre ++ "@Francis-VAIO", list_to_atom(Name) end, Node_Numbers).

organise_distributed_configs(Nodes,Process_Num) ->
	List_Network_Config = #list_network_config{network_size=Process_Num,
                    		work_sharing_threshold=2,
                    		work_splitting_fun=fun(List)->util:split_alternate(List)end},
	List_Network_Configs = util:generate_numbered_list(fun(Num) -> 
															   erlang:setelement(#list_network_config.log_name,List_Network_Config,util:format_int(Num)++"_list_log.LOG") 
													   end, length(Nodes)),
	#distributed_network_config{nodes=Nodes,list_network_configs=List_Network_Configs}.

analyse_and_print_results(Results) ->
	Extract_Time = fun({_Name,Time,_Solved}) ->
						   Time
				   end,
	Extract_Solved = fun({_Name,_Time,Solved}) ->
						   Solved
				   end,
	Extract_Name = fun({Name,_Time,_Solved}) ->
						   Name
				   end,
	Times = lists:map(fun(Run)-> lists:map(Extract_Time,Run) end,Results),
	Solveds = lists:map(fun(Run)-> lists:map(Extract_Solved,Run) end,Results),
	Names = lists:map(Extract_Name,hd(Results)),
	io:format("~nRaw Times! ~n~n~w~n",[lists:flatten(Times)]),
	io:format("~n~n*************************************~n~n"),
	Means = stats_util:mean_vertical_2d(Times),
	Named_Means = lists:zip(Names, Means),
	Compare_Performance = fun(Run) ->
							  Run_Mean = stats_util:mean(Run),
							  lists:map(fun(Run_Time) -> Run_Time/Run_Mean end, Run)
					  end,
	Performance_Comparisons = lists:map(Compare_Performance, Times),
	io:format("~n~n~p~nResults = ~n~p~n~nMeans = ~n~p~n~nPerformance Comparisons = ~p~n~n",[Names,Results,Named_Means,Performance_Comparisons]),
	ok.

%%
%%	
%%
test(Formula) ->
    Generated_Formula = {generated_formula,Formula,undefined,undefined},
    Dp_Funs = most_common_first_dp:get_functions(),
    Default_Args = dp_engine_concurrent:get_default_args(),
    Process_Tree_Args = erlang:setelement(#engine_concurrent_args.process_tree_mod,Default_Args,process_tree),
    Dp_Funs_PT = erlang:setelement(#dp_funs.engine_args,Dp_Funs,Process_Tree_Args),
    Fun = fun() -> compare_for_formulae([Generated_Formula],[{"Con = ",Dp_Funs_PT,dp_engine_concurrent}]) end,
    util:do_n_times(Fun,1).

%%
%%	Returns a [{#generated_formula[{String(),Integer(),Bool()}]}]
%%
compare_for_formulae(Generated_Formulae,Names_And_Dp_Funs) ->
	Solve = 	fun(Generated_Formula) ->
                    Solve_And_Print = fun({Name,Dp_Funs,Engine_Mod}) ->
                                  			{Time,Sat_Result} = test_util:solve_and_time(Dp_Funs,Generated_Formula,Engine_Mod),
                                            %%io:format("~n~nSat_Result = ~p~n",[Sat_Result]),
                                            %%io:format("~n~n~p~p",[Name,util:format_int(Time)]),
                                  			{Name,Time,Sat_Result#sat_result.satisfiable}
                         			  end,
                    Formula_Run = lists:map(Solve_And_Print,Names_And_Dp_Funs),
                    sanity_check(Formula_Run,Generated_Formula),
                    Formula_Run
				end,
    lists:map(Solve,Generated_Formulae).

%%
%%	Creates a file "C:/Sat_Test/formulae/formula_file.txt" containing a list of formulae
%%	which will be used by the run/0 function.
%%	
generate_formulae_file(Formula_Num) ->
    generate_formulae_file(Formula_Num,150,[642,640,641]).

generate_formulae_file(Formula_Num,Var_Num,Clause_Num) ->
    formula_util:make_formulae_file(Var_Num,Clause_Num,3,Formula_Num,?TEST_FORMULAE_FILE_NAME,?TEST_FORMULAE_FILE_PATH).
  
%%
%%	Calls get run times with profiling
%%
profile_run_times(Node_Num,Process_Num) ->
    eprof:profile([Node_Num,Process_Num],fun run/2),
    eprof:analyse().

%%
%%	Tests the speed of randomly assigning variables to a formula data-structure
%%
test_assignments() ->
    %% Use these variables to determine the formula datastructure
    Data_Structure = ?FORMULA_DATA_STRUCTURE,
    %% The actual test below
    Generated_Formulae = formula_util:consult_formulae_file(?TEST_FORMULAE_FILE_NAME,?TEST_FORMULAE_FILE_PATH),
    Generated_Formula = hd(Generated_Formulae),
    Formula = Data_Structure:def_2_formula(Generated_Formula#generated_formula.formula_def),
    Do_Assignment = fun() ->
							Variables = Data_Structure:get_variables(Formula),
                            [Variable] = util:random_sublist(1,Variables),
                            Boolean = util:int_to_bool(random:uniform(9)),
                            Variable_Assignment = #variable_assignment{l_name=Variable,value=Boolean},
                            ?FORMULA_DATA_STRUCTURE:assign_variable(Variable_Assignment,Formula)
                    end,
    {Time,_Return} = timer:tc(util,do_n_times,[Do_Assignment,1000]),
    util:format_int(Time).

%%
%%	Tests the speed of assigning each variable in a formula data-structure until all
%%	variables have been assigned.
%%
test_ass_all() ->
    %% Use these variables to determine the formula datastructure
    Data_Structure = cnf_graph_array,
    %% The actual test below
    Generated_Formulae = formula_util:consult_formulae_file(?TEST_FORMULAE_FILE_NAME,?TEST_FORMULAE_FILE_PATH),
    Generated_Formula = hd(Generated_Formulae),
    Formula = Data_Structure:def_2_formula(Generated_Formula#generated_formula.formula_def),
    Variable_Names = Data_Structure:get_variables(Formula),
    Assignment_Counts = lists:seq(1,length(Variable_Names)),
    Names_And_Counts = lists:zip(Variable_Names,Assignment_Counts),
    Assignment_Fun = fun({Variable_Name,Assignment_Count},Acc_Formula) ->
                            Do_Assignment = fun() ->
                                                    Boolean = util:int_to_bool(random:uniform(9)),
                            						Variable_Assignment = #variable_assignment{l_name=Variable_Name,value=Boolean},
                           		 					?FORMULA_DATA_STRUCTURE:assign_variable(Variable_Assignment,Acc_Formula)
                                            end,
                            util:do_n_times(Do_Assignment,math:pow(2,Assignment_Count)),
                      		Boolean = util:int_to_bool(random:uniform(9)),
                            Variable_Assignment = #variable_assignment{l_name=Variable_Name,value=Boolean},
                           	?FORMULA_DATA_STRUCTURE:assign_variable(Variable_Assignment,Acc_Formula)
                    end,
    Single_Run = fun() ->
                        lists:foldl(Assignment_Fun,Formula,Names_And_Counts)
                 end,
    {Time,_Return} = timer:tc(util,do_n_times,[Single_Run,1]),
    io:format("~n~n~p~n~n",[_Return]),
    util:format_int(Time).

%%
%%
%%
generate(Var_Num,C_Num,C_Size,Formula_Num) ->
    formula_generator:generate_formulae(Var_Num,C_Num,C_Size,Formula_Num).

%%
%%
%%
sanity_check(Formula_Run,Generated_Formula) ->
	All_Sats = [Sat||{_Name,_Time,Sat}<-Formula_Run],
	case util:homogenous_list(All_Sats) of
		false
	    	->	io:format("~n~nFuck!!!!!!!!!!!!!!~n~nTwo solvers have different opinions about a formula~n~nSolver results = ~p~n~n",[Formula_Run]),
	        	print_formula_to_file(Generated_Formula);
	    true
	    	->	ok
	end.

%%
%%	
%%
print_formula_to_file(Generated_Formula) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    Random_Number = util:format_int(random:uniform(1000)),
    {ok,File} = util:make_file(Random_Number ++ "Error_Formula.txt",test_util:get_file_path() ++ "/error"),
    {Module,F_Name} = (Generated_Formula#generated_formula.print_generated_formula),
	Module:F_Name(File,Generated_Formula),
	file:close(File).


run_quick_test() ->
    Run_Sizes = [1,20,40,60,80,100,200,300,400,500,1000,1500,2000,2500,3000,3500,4000,4500,5000],
    Fold_Fun = fun(Run_Size,Dict) ->
                       Result = integer_test(Run_Size),
                       dict:store(Run_Size,Result,Dict)
               end,
    Results_Dict = lists:foldl(Fold_Fun,dict:new(),Run_Sizes),
    io:format("Size, Dictionary, Array, Set, My_List, List~n"),
    Print_Results = fun(Run_Size) ->
                            {Dict,Array,Set,My_List,List} = dict:fetch(Run_Size,Results_Dict),
                            io:format("~p, ~p, ~p, ~p, ~p, ~p~n",[Run_Size,Dict,Array,Set,My_List,List])
                    end,
    lists:foreach(Print_Results,Run_Sizes).
    
atom_test(Max) ->
    Iterations = 500000,
    Number_List = lists:seq(1,Max),
    String_List = lists:map(fun util:format_int/1,Number_List),
    Atom_List = lists:map(fun list_to_atom/1,String_List),
    Self_Dict = fun(Number,Number_Dict) ->
                       String = util:format_int(Number),
                       Atom = list_to_atom(String),
                       dict:store(Atom,Atom,Number_Dict)
                end,
    Test_Dict = lists:foldl(Self_Dict,dict:new(),Number_List),
    Self_Array = fun(Number,Number_Array) ->
                       String = util:format_int(Number),
                       Atom = list_to_atom(String),
                       array:set(Number,Atom,Number_Array)
                 end,
    Test_Array = lists:foldl(Self_Array,array:new(),Number_List),
    Access_Dict = fun() ->
                     Number = random:uniform(Max),
                     String = util:format_int(Number),
                     Atom = list_to_atom(String),
                     Atom = dict:fetch(Atom,Test_Dict)
             end,
    {Time_Dict,_Return_Dict} = timer:tc(util,do_n_times,[Access_Dict,Iterations]),
    Access_Array = fun() ->
                     Number = random:uniform(Max),
                     String = util:format_int(Number),
                     Atom = list_to_atom(String),
                     Atom = array:get(Number,Test_Array)
             end,
    {Time_Array,_Return_Array} = timer:tc(util,do_n_times,[Access_Array,Iterations]),
    Access_List = fun() ->
                     Number = random:uniform(Max),
					 String = util:format_int(Number),
                     Atom = list_to_atom(String),
                     true = lists:member(Atom,Atom_List)
               end,
    {Time_List,_Return_List} = timer:tc(util,do_n_times,[Access_List,Iterations]),
    io:format("~n~nReturn Dict = ~p~n~nReturn Array = ~p~n~nReturn List = ~p~n~n",[_Return_Dict,_Return_Array,_Return_List]),
    {Time_Dict,Time_Array,Time_List}.

string_test(Max) ->
    Iterations = 500000,
    Number_List = lists:seq(1,Max),
    String_List = lists:map(fun util:format_int/1,Number_List),
    Self_Dict = fun(Number,Number_Dict) ->
                       String = util:format_int(Number),
                       dict:store(String,String,Number_Dict)
                end,
    Test_Dict = lists:foldl(Self_Dict,dict:new(),Number_List),
    Self_Array = fun(Number,Number_Array) ->
                       String = util:format_int(Number),
                       array:set(Number,String,Number_Array)
                 end,
    Test_Array = lists:foldl(Self_Array,array:new(),Number_List),
    Access_Dict = fun() ->
                     Number = random:uniform(Max),
                     String = util:format_int(Number),
                     String = dict:fetch(String,Test_Dict)
             end,
    {Time_Dict,_Return_Dict} = timer:tc(util,do_n_times,[Access_Dict,Iterations]),
    Access_Array = fun() ->
                     Number = random:uniform(Max),
                     String = util:format_int(Number),
                     String = array:get(Number,Test_Array)
             end,
    {Time_Array,_Return_Array} = timer:tc(util,do_n_times,[Access_Array,Iterations]),
    Access_List = fun() ->
                     Number = random:uniform(Max),
					 String = util:format_int(Number),
                     true = lists:member(String,String_List)
               end,
    {Time_List,_Return_List} = timer:tc(util,do_n_times,[Access_List,Iterations]),
    io:format("~n~nReturn Dict = ~p~n~nReturn Array = ~p~n~nReturn List = ~p~n~n",[_Return_Dict,_Return_Array,_Return_List]),
    {Time_Dict,Time_Array,Time_List}.

integer_test(Max) ->
    Iterations = 500000,
    Number_List = lists:seq(1,Max),
    Degree_Of_Error = 1,
    Self_Dict = fun(Number,Number_Dict) ->
                       dict:store(Number,Number,Number_Dict)
                end,
    Test_Dict = lists:foldl(Self_Dict,dict:new(),Number_List),
    Self_Array = fun(Number,Number_Array) ->
                       array:set(Number,Number,Number_Array)
                 end,
    Test_Array = lists:foldl(Self_Array,array:new(Max+1),Number_List),
    Make_Set = fun(Number,Number_Set) ->
                       sets:add_element(Number,Number_Set)
               end,
    Test_Set = lists:foldl(Make_Set,sets:new(),Number_List),
    Access_Dict = fun() ->
                     Number = random:uniform(Max*Degree_Of_Error),
                     dict:find(Number,Test_Dict)
             end,
    {Time_Dict,_Return_Dict} = timer:tc(util,do_n_times,[Access_Dict,Iterations]),
    Access_Array = fun() ->
                     Number = random:uniform(Max*Degree_Of_Error),
                     array:get(Number,Test_Array)
             end,
    {Time_Array,_Return_Array} = timer:tc(util,do_n_times,[Access_Array,Iterations]),
    Access_Set = fun() ->
                     Number = random:uniform(Max*Degree_Of_Error),
                     sets:is_element(Number,Test_Set)
             end,
    {Time_Set,_Return_Set} = timer:tc(util,do_n_times,[Access_Set,Iterations]),
    Access_List = fun() ->
                     Number = random:uniform(Max*Degree_Of_Error),
                     lists:member(Number,Number_List)
               end,
    {Time_List,_Return_List} = timer:tc(util,do_n_times,[Access_List,Iterations]),
    Access_My_List = fun() ->
                     Number = random:uniform(Max*Degree_Of_Error),
                     my_member(Number,Number_List)
               end,
    {Time_My_List,_Return_My_List} = timer:tc(util,do_n_times,[Access_My_List,Iterations]),
    io:format("~n~nReturn Dict = ~p~n~nReturn Array = ~p~n~nReturn Set = ~p~n~nReturn My List = ~p~n~nReturn List = ~p~n~n",[_Return_Dict,_Return_Array,_Return_Set,_Return_My_List,_Return_List]),
    {Time_Dict,Time_Array,Time_Set,Time_My_List,Time_List}.

my_member(_Elem,[]) ->
    false;

my_member(Elem,[Elem|_List]) ->
    true;

my_member(Elem,[_Head|List]) ->
    my_member(Elem,List).
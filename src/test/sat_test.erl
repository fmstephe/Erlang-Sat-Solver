%% Author: Francis Stephens
%% Created: 4 Dec 2007
%% Description: Battery of tests for sat solver programs
-module(sat_test).

%%%
%%%	DP strategy macro
%%%
%%%	This macro is used to universally replace the davis putnam
%%%	strategy used to solve sat instances.
%%%
%%%	Strategies include:
%%%		1: most_comon_first_dp
%%%		2: naive_dp
%%%		3: dp_basic
%%%
-define(DP_STRATEGY,naive_dp).

%%
%%	Records
%%

%%
%%	Exported Functions
%%
-export([run_many/4,run_many/3,run_single/3,run_single/2,static_tests/1,is_validly_satisfied/1]).
-export([retrieve_literals/1,test_sat/1]).

-compile(export_all).

%%
%%	Include files
%%

-include("formulae.hrl").
-include("process_records.hrl").
-include("sat_records.hrl").

%%
%% API Functions
%%

%%
%% Runs a series of static tests
%%
static_tests(Print_Results) ->
	test_sat(Print_Results).

%%
%%	Tests the tree aggregator engine
%%
test_it(Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    test_it(concise,Var_Num_Roof,Con_Num_Roof,Formula_Num).

test_it(single,Formula_Fun) ->
	Funs = ?DP_STRATEGY:get_functions(),
	Formula = ?FORMULA_DATA_STRUCTURE:def_2_formula({formula_def,apply(sat_test,Formula_Fun,[])}),
	Sat_Result = dp_engine_iterative:is_satisfiable(Funs,Formula),
	io:format("~n~n******************~p*************************~n~n",[Sat_Result#sat_result.satisfiable]).

test_it(profile,Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    eprof:profile([],fun()->test_it(clear,Var_Num_Roof,Con_Num_Roof,Formula_Num)end),
	eprof:analyse();

test_it(Print_Results,Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    %%Start the tree aggregating server.
	Search_Fun = fun(Formula) ->
		Funs = ?DP_STRATEGY:get_functions(),
		dp_engine_iterative:is_satisfiable(Funs,Formula)
	end,
	Eval_Shell = fun(Formulae) ->
		[Search_Fun(Formula)||Formula <- Formulae]
	end,
    {Time1,Premade} = timer:tc(sat_test,test_premade,[Print_Results,Eval_Shell]),
    {Time2,Dynamic} = timer:tc(sat_test,test_dynamic,[Print_Results,Eval_Shell,Var_Num_Roof,Con_Num_Roof,Formula_Num]),
  	io:format("~nSat test succeeded = ~p~n",[Premade and Dynamic]),
	io:format("~nSat test time elapsed =~n     ~p (in microseconds)~n    ~p (in seconds)~n~n~n",[Time1+Time2,(Time1+Time2)/1000000]).

%%
%%	Tests the tree aggregator engine
%%
test_agg(Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    test_agg(concise,Var_Num_Roof,Con_Num_Roof,Formula_Num).

test_agg(single) ->
	Funs = ?DP_STRATEGY:get_functions(),
	Formula_Def = sat4(),
	Generated_Formula = #generated_formula{	formula_def=Formula_Def,
											creational_metadata=dict:new(),
											print_generated_formula={formula_util,print_generated_formula}},
    dp_engine_tree_agg:is_satisfiable(Funs,Generated_Formula).

test_agg(profile,Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    eprof:profile([],fun()->test_agg(clear,Var_Num_Roof,Con_Num_Roof,Formula_Num)end),
	eprof:analyse();

test_agg(Print_Results,Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    %%Start the tree aggregating server.
    {ok,Aggregator_Pid} = gen_server:start(tree_agg_callback,[],[{timeout,infinity}]),
	Search_Fun = fun(Formula_Def) ->
		Funs = ?DP_STRATEGY:get_functions(),
		Generated_Formula = #generated_formula{	formula_def=Formula_Def,
												creational_metadata=dict:new(),
												print_generated_formula={formula_util,print_generated_formula}},
        dp_engine_tree_agg:is_satisfiable(Funs,Generated_Formula)
	end,
	Eval_Shell = fun(Formulae) ->
		[Search_Fun(Formula)||Formula <- Formulae]
	end,
    {Time1,Premade} = timer:tc(sat_test,test_premade,[Print_Results,Eval_Shell]),
    {Time2,Dynamic} = timer:tc(sat_test,test_dynamic,[Print_Results,Eval_Shell,Var_Num_Roof,Con_Num_Roof,Formula_Num]),
  	io:format("~nSat test succeeded = ~p~n",[Premade and Dynamic]),
	io:format("~nSat test time elapsed =~n     ~p (in microseconds)~n    ~p (in seconds)~n~n~n",[Time1+Time2,(Time1+Time2)/1000000]).

%%
%%	Set of tests ensuring that satisfiable formulae are found to be satisfiable
%%	and that unsatisfiable formulae are found to be unsatisfiable
%%
test_sat() ->
    test_sat(concise).

test_sat(profile) ->
    eprof:profile([],fun()->test_sat(clear)end),
	eprof:analyse();

test_sat(premade) ->
    {Time1,Premade} = timer:tc(sat_test,test_premade,[concise]),
  	io:format("~nSat test succeeded = ~p~n",[Premade]),
	io:format("~nSat test time elapsed =~n     ~p (in microseconds)~n    ~p (in seconds)~n~n~n",[Time1,(Time1)/1000000]).

test_sat(Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
	Eval_Shell = fun(Formulae) ->
		Funs = ?DP_STRATEGY:get_functions(),
		lists:map(fun(Formula)->(dp_engine:is_satisfiable(Funs,Formula))#sat_result.satisfiable end,Formulae)
	end,
    {Time1,Premade} = timer:tc(sat_test,test_premade,[concise,Eval_Shell]),
    {Time2,Dynamic} = timer:tc(sat_test,test_dynamic,[concise,Eval_Shell,Var_Num_Roof,Con_Num_Roof,Formula_Num]),
  	io:format("~nSat test succeeded = ~p~n",[Premade]),
	io:format("~nSat test time elapsed =~n     ~p (in microseconds)~n    ~p (in seconds)~n~n~n",[Time1+Time2,(Time1+Time2)/1000000]).

test_premade(Print_Results,Eval_Shell) ->
    Satisfied_Results = Eval_Shell(satisfiable_formulae()),
	Unsatisfied_Results = Eval_Shell(unsatisfiable_formulae()),
	case Print_Results of
		concise 
        	->	io:format("~nTrue premade formulae ~p~n~n",[Satisfied_Results]),
				io:format("~nFalse premade formulae ~p~n~n",[Unsatisfied_Results]),
               	ternary_logic:all_true3(Satisfied_Results) and ternary_logic:all_false3(Unsatisfied_Results);
		clear
			->	ternary_logic:all_true3(Satisfied_Results) and ternary_logic:all_false3(Unsatisfied_Results);
		_Other
			->	ternary_logic:all_true3(Satisfied_Results) and ternary_logic:all_false3(Unsatisfied_Results)
	end.

test_dynamic(Print_Results,Eval_Shell,Var_Num_Roof,Con_Num_Roof,Formula_Num) ->
    Sat_Fun = fun()->Exp=boolean_expression:make_random_tautology(random:uniform(Var_Num_Roof),random:uniform(Con_Num_Roof)),boolean_expression:export_expression(Exp) end,
    Unsat_Fun = fun()->Exp=boolean_expression:make_random_unsat(random:uniform(Var_Num_Roof),random:uniform(Con_Num_Roof)),boolean_expression:export_expression(Exp) end,
	{A1,A2,A3} = now(),
	random:seed(A1,A2,A3),
    Sat_List = util:generate_list(Sat_Fun,Formula_Num),
	{A4,A5,A6} = now(),
	random:seed(A4,A5,A6),
	Unsat_List = util:generate_list(Unsat_Fun,Formula_Num),
    Satisfied_Results = Eval_Shell(Sat_List),
	Unsatisfied_Results = Eval_Shell(Unsat_List),
	case Print_Results of
		concise 
        	->	io:format("~nTrue dynamic formulae ~p~n~n",[Satisfied_Results]),
				io:format("~nFalse dynamic formulae ~p~n~n",[Unsatisfied_Results]),
                ternary_logic:all_true3(Satisfied_Results) and ternary_logic:all_false3(Unsatisfied_Results);
		clear
			->	ternary_logic:all_true3(Satisfied_Results) and ternary_logic:all_false3(Unsatisfied_Results);
		_Other
			->	ternary_logic:all_true3(Satisfied_Results) and ternary_logic:all_false3(Unsatisfied_Results)
	end.

%%	
%% 	Tests many,F_Num,CNF formulae for satisfiability
%%		V_Num:
%%			The number of variables available for formula construction
%%		C_Num:
%%			The number of clauses in each formula (clause size is fixed |C| = C_Size)
%%		C_Size:
%%			The size of each clause
%%		F_Num:
%%			The number of formula to test
%%
run_many(V_Num,C_Num,C_Size,F_Num) -> 
	Formulae = lists:map(fun naive_dp:is_satisfiable/1, formula_generator:generate_formulae(V_Num,C_Num,C_Size,F_Num)),
	lists:map(fun is_validly_satisfied/1, Formulae).

%%
%% 	Tests many, F_Num, CNF formulae for satisfiability
%%		V_Num:
%%			The number of variables available for formula construction
%%		C_Num:
%%			The number of clauses in each formula (the size of each clause is random 1 <= |C| <= Var_Num) 
%%		F_Num:
%%			The number of formula to test
%%
run_many(V_Num,C_Num,F_Num) ->
	Formulae = lists:map(fun naive_dp:is_satisfiable/1, formula_generator:generate_formulae(V_Num,C_Num,F_Num)),
	lists:map(fun is_validly_satisfied/1, Formulae).
%%	
%% 	Tests a single CNF formula for satisfiability
%%		V_Num:
%%			The number of variables available for formula construction
%%		C_Num:
%%			The number of clauses in each formula (clause size is fixed |C| = C_Size)
%%		C_Size:
%%			The size of each clause
%%
run_single(V_Num,C_Num,C_Size) -> 
	is_validly_satisfied(naive_dp:is_satisfiable(formula_generator:generate_formula(V_Num,C_Num,C_Size))).
%%	
%% 	Tests a single CNF formula for satisfiability
%%		V_Num:
%%			The number of variables available for formula construction
%%		C_Num:
%%			The number of clauses in each formula (the size of each clause is random 1 <= |C| <= Var_Num) 
%%
run_single(V_Num,C_Num) -> 
	is_validly_satisfied(naive_dp:is_satisfiable(formula_generator:generate_formula(V_Num,C_Num))).


%%
%% Local Functions
%%

is_validly_satisfied({sat_result,true,Formula}) ->
	all_validly_subsumed(Formula);

is_validly_satisfied({sat_result,false,Formula}) ->
	all_validly_subsumed(Formula).

%%
%%	Creates a list containing all the literals, including repititions, from a formula
%%
%%		Formula:
%%			#formula
%%

retrieve_literals({formula,_Variable_Dict,Clauses}) ->
	[Literal||{clause,Literals,_C_Flag} <- Clauses, Literal <- Literals].

%%
%%	Indicates whether each subsumed clause in a formula is valid
%%	A clause may be subsumed iff at least one of it's literals evaluates to true
%%
all_validly_subsumed(Formula) ->
	Clauses = ?FORMULA_DATA_STRUCTURE:get_clauses(Formula),
	ternary_logic:all_true3(lists:map(fun (Clause) -> validly_subsumed_clause(Clause,Formula) end,Clauses)).

%%
%%	Indicates whether a given clause is validly subsumed
%%	A clause may be subsumed iff at least one of it's literals evaluates to true
%%
validly_subsumed_clause(Clause,Formula) ->
	Literals = ?FORMULA_DATA_STRUCTURE:get_literals(Clause),
	util:one_true(lists:map(fun (Literal) -> ?FORMULA_DATA_STRUCTURE:eval_literal(Literal,Formula) end, Literals)).
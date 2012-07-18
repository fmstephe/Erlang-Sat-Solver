%% Author: Francis Stephens
%% Created: 28 Jan 2009
%% Description: TODO: Add description to test_util
-module(test_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get_file_path/0,solve_and_time/3]).

%%
%% API Functions
%%

%%
%%	Returns the current root folder into which test output should be written.
%%
get_file_path() ->
    "C:/Sat_Test".

%%
%%	Returns a tuple {Time,Sat_Result} where the Time indicates the time taken to solve the #generated_formula
%%	and Sat_Result is the #sat_result returned by the solver engine.
%%
%%	Generated_Formula: The #generated_formula to be solved
%%		#generated_formula
%%	Dp_Funs: The #dp_funs used to solve the #generated_formula
%%		#dp_funs
%%	Engine_Mod: The module of the solver engine to be used
%%		Atom()
%%
solve_and_time(Dp_Funs,Generated_Formula,Engine_Mod) ->
    {Time,Sat_Result} = timer:tc(Engine_Mod,is_satisfiable,[Dp_Funs,Generated_Formula]),
	{Time,Sat_Result}.

%% Author: Francis Stephens
%% Created: 26 Jan 2009
%% Description: TODO: Add description to tree_metadata
-module(tree_metadata_util).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("process_records.hrl").

%%
%% Exported Functions
%%
-export([print_to_file/2]).

%%
%% API Functions
%%

%%
%%	Prints the #tree_metric to a file which will be created on the path provided.
%%	This function will generate the file name automatically including the identifier
%%	provided.
%%
%%	Tree_Metadata: The #tree_metadata to print.
%%		#tree_metadata
%%	File_Path: A file-path defining the location where the generated formula file will be located
%%		String()
%%
print_to_file(Tree_Metadata,File_Path) ->
    {ok,File} = util:make_file("Tree_Metadata.txt",File_Path),
	Generated_Formula = Tree_Metadata#tree_metadata.generated_formula,
	Solver_Strategy = Tree_Metadata#tree_metadata.strategy_name,
    {Module,F_Name} = (Generated_Formula#generated_formula.print_generated_formula),
	Module:F_Name(File,Generated_Formula),
	print_strategy(File,Solver_Strategy),
	file:close(File).

%%
%% Local Functions
%%

print_strategy(IO_Device,Solver_Strategy) ->
	io:format(IO_Device,"~n~nStrategy = ~p~n~n",[Solver_Strategy]).
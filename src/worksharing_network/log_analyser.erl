%% Author: Francis Stephens
%% Created: 14 Nov 2009
%% Description: TODO: Add description to log_analyser
-module(log_analyser).

%%
%% Include files
%%

%%
%%	Macros
%%

-define(STAR_LINE,"************************************************************").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

analyse_logs(Log_Files) ->
	File_Processing = fun (File) ->
							   file_util:list_lines(File)
					  end,
	Files_As_Lines = lists:map(fun(Log_File) -> file_util:process_file(Log_File) end, Log_Files).
	

%%
%% Local Functions
%%


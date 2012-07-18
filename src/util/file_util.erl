%% Author: Francis Stephens
%% Created: 14 Nov 2009
%% Description: TODO: Add description to file_util
-module(file_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([process_file/2,list_lines/1]).

%%
%% API Functions
%%

process_file(FilePath,File_Fun) ->
	case file:open(FilePath,read) of
		{ok,Val} -> 
			Processing_Result = File_Fun(FilePath),
			file:close(Val),
			Processing_Result;
		{error,Why} ->
			file:close(FilePath),
			{error,Why}
	end.

%%
%%	Reads every line from a file into a list
%%
%%	File_Val:
%%		An opened file
%%
list_lines(File_Val) ->
	case io:get_line(File_Val,'') of
		eof ->
			[];
		Line ->
			[Line|list_lines(File_Val)]
	end.

%%
%% Local Functions
%%


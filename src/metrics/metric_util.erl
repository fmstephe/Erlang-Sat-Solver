%% Author: Francis Stephens
%% Created: 21 Nov 2008
%% Description: TODO: Add description to metric_util
-module(metric_util).

%%
%% Include files
%%

-include("process_records.hrl").

%%
%% Exported Functions
%%
-export([print_to_file/3,print_to_file/2,r_format_lists/2,r_format_lists/3]).

%%
%% API Functions
%%

%%
%%	Prints the #tree_metric to a file which will be created on the path provided.
%%	This function will generate the file name automatically.
%%
%%	Tree_Metric: The #tree_metric to print.
%%		#tree_metric
%%	File_Path: A file-path defining the location where the tree metric file will be located
%%		String()
%%
print_to_file(Tree_Metric,File_Path) -> 
	print_to_file(Tree_Metric,"",File_Path).

%%
%%	Prints the #tree_metric to a file which will be created on the path provided.
%%	This function will generate the file name automatically including the identifier
%%	provided.
%%
%%	Tree_Metric: The #tree_metric to print.
%%		#tree_metric
%%	Identifier: A special convenience identifier to add to the file-name
%%		String()
%%	File_Path: A file-path defining the location where the tree metric file will be located
%%		String()
%%
print_to_file(Tree_Metric,Identifier,File_Path) -> 
    Metric_Type = Tree_Metric#tree_metric.metric_type,
	Tree_Id = Tree_Metric#tree_metric.tree_id,
	File_Name = Metric_Type ++ "_on_" ++ [util:int_to_string(Tree_Id)] ++ Identifier ++ ".mtc",
	{ok,File} = util:make_file(File_Name,File_Path),
    (Tree_Metric#tree_metric.print_metric)(File,Tree_Metric),
	file:close(File).

%%
%%	As a side-effect this function will print the list of lists
%%	provided in the manner required by R.  Each list will be printed
%%	as a series of vertical columns where each column is seperated by
%%	tabs.
%%	This function assumes that every list is of the same length.  If 
%%	this is not true this function may fail.
%%
%%	Lists: A list of lists to be formatted
%%		[[Term()]]
%%	IO_Device: The io_device() to which we will print
%%		io_device()
%%
r_format_lists(Lists,IO_Device) ->
    Tab = "	",
	Heads = util:heads(Lists),
	Tails = util:tails(Lists),
	format_header_line(Heads,Tab,IO_Device),
	r_format_lists_int(Tails,Tab,IO_Device).

%%
%%	As a side-effect this function will print the list of lists
%%	provided in the manner required by R.  Each list will be printed
%%	as a series of vertical columns where each column is seperated by
%%	a seperator character (probably a tab or series of spaces).
%%	This function assumes that every list is of the same length.  If 
%%	this is not true this function may fail.
%%
%%	Lists: A list of lists to be formatted
%%		[[Term()]]
%%	Seperator: Some characters acting as a column seperator
%%		String()
%%	IO_Device: The io_device() to which we will print
%%		io_device()
%%
r_format_lists(Lists,Seperator,IO_Device) ->
	Heads = util:heads(Lists),
	Tails = util:tails(Lists),
	format_header_line(Heads,Seperator,IO_Device),
	r_format_lists_int(Tails,Seperator,IO_Device).

r_format_lists_int([[]|_Lists],_Seperator,_IO_Device) ->
	ok;

r_format_lists_int(Lists,Seperator,IO_Device) ->
	Heads = util:heads(Lists),
	Tails = util:tails(Lists),
	format_line(Heads,Seperator,IO_Device),
	r_format_lists_int(Tails,Seperator,IO_Device).

%%
%%	As a side-effect this function will print the list provided on a single
%%
%%	List: A list to be formatted
%%		[Term()]
%%	Seperator: Some characters acting as a column seperator
%%		String()
%%	IO_Device: The io_device() to which we will print
%%		io_device()
%%
format_header_line([Last_Element|[]],_Seperator,IO_Device) ->
	io:format(IO_Device,"~s~n",[Last_Element]);

format_header_line([Element|Elements],Seperator,IO_Device) ->
	io:format(IO_Device,"~s~s",[Element,Seperator]),
	format_header_line(Elements,Seperator,IO_Device).

%%
%%	As a side-effect this function will print the list provided on a single
%%
%%	List: A list to be formatted
%%		[Term()]
%%	Seperator: Some characters acting as a column seperator
%%		String()
%%	IO_Device: The io_device() to which we will print
%%		io_device()
%%
format_line([Last_Element|[]],_Seperator,IO_Device) ->
	io:format(IO_Device,"~w~n",[Last_Element]);

format_line([Element|Elements],Seperator,IO_Device) ->
	io:format(IO_Device,"~w~s",[Element,Seperator]),
	format_line(Elements,Seperator,IO_Device).

%%
%% Local Functions
%%

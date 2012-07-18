%% Author: Francis Stephens
%% Created: 25 Jul 2008
%% Description: TODO: Add description to persistence_util
-module(persistence_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_mnesia/0,create_schema/0,create_table/3]).
-compile(export_all).

%%
%% API Functions
%%

%%
%%	Starts Mnesia if it is not already running
%%	NB: It is useful to observe that you can't create a schema once mnesia is running
%%
start_mnesia() ->
	case mnesia:system_info(is_running) of
		no  ->	ok = mnesia:start();
		yes -> 	ok
	end.

%%
%%	Creates a schema if one does not already exist
%%	NB: It is useful to observe that you can't create a schema once mnesia is running
%%
%%	This current implementation is god-aweful.  I don't know how to determine whether or not
%%	a schema already exists and so I just create one and ignore errors.
%%	Two likely candidates for determining this are
%%		1: mnesia:system_info(db_nodes)
%%		2: mnesia:system_info(schema_location)
%%	But they both seem to return the same thing regardless of the existence of a schema?!?
%%
create_schema() ->
	mnesia:create_schema([node()]).

%%
%%	Creates a new table with given name and attributes if a table
%%	of that name does not already exist
%%
%%	Table_Name:
%%			The name of the table to be (conditionally) created
%%		String()
%%	Table_Attributes:
%%			List of columns for the table.
%%			length(TableAttributes) >= 2.
%%		[String()]
%%
create_table(Table_Name,Table_Attributes,Additional_Options) ->
	Local_Tables = mnesia:system_info(local_tables),
	case lists:member(Table_Name,Local_Tables) of
		false
			->	{atomic, ok} = mnesia:create_table(Table_Name,[{attributes,Table_Attributes}|Additional_Options]);
		true
			->	ok
	end.

%%
%% Local Functions
%%
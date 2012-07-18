%% Author: Francis Stephens
%% Created: 22 Oct 2008
%% Description: TODO: Add description to network_worker
-module(network_worker).

%%
%% Include files
%%

-include("worksharing_records.hrl").
-include("process_tree_records.hrl").

%%
%% Exported Functions
%%
-export([]).

-compile(export_all).

%%
%% API Functions
%%

%%
%% Local Functions
%%

start() ->
    Network_Config = process_tree_util:get_default_config(),
    Network_Config2 = setelement(#network_config.work_sharing_threshold,Network_Config,1),
    Network_Config3 = setelement(#network_config.process_spawn_threshold,Network_Config2,1),
    Network_Info = process_tree:create_worksharing_network(process_tree_util:get_default_config()),
	loop(Network_Info,[1,2,3,4,5,6,7,8]).

accept_delegation(Network_Info,Workstack) ->
    %%io:format("~n~nI(~p) have just been born. My workstack is ~p~n~n",[self(),Workstack]),
	loop(Network_Info,Workstack).

handle_no_work() ->
    io:format("~n~n(~p) Panic there is no work for anyone this search has failed!~n~n",[self()]).

loop(Network_Info,[X]) ->
	io:format("~n~n(~p) I have completed this is my number ~p~n~nNode Count = ~p~n~n",[self(),X,Network_Info#network_info.process_count]),
	Process_Delegation = #process_delegation{module=network_worker,delegation_fun=accept_delegation,delegation_args=[],no_work_fun=handle_no_work,no_work_args=[]},
	Network_Workstack = process_tree:manage_network(Network_Info,[],Process_Delegation);
	%%loop(Network_Workstack#network_workstack.network_info,Network_Workstack#network_workstack.workstack);

loop(Network_Info,Workstack) ->
    io:format("~n~n(~p) Node Count = ~p~nWorkstack = ~p~n~n",[self(),Network_Info#network_info.process_count,Workstack]),
    timer:sleep(3000),
	Process_Delegation = #process_delegation{module=network_worker,delegation_fun=accept_delegation,delegation_args=[],no_work_fun=handle_no_work,no_work_args=[]},
	Network_Workstack = process_tree:manage_network(Network_Info,Workstack,Process_Delegation),
	loop(Network_Workstack#network_workstack.network_info,Network_Workstack#network_workstack.workstack).

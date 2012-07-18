%% Author: Francis Stephens
%% Created: 13 Oct 2008
%% Description: TODO: Add description to process_tree
-module(process_tree_light).

%%
%% Include files
%%

-include("process_tree_records.hrl").

%%
%% Exported Functions
%%
-export([create_worksharing_network/2,manage_network/3,work_complete/2]).

%%
%% API Functions
%%

%%
%%	Returns a new #network_info representing the root of a new process tree.
%%
%%	Network_Config: A #network_config contains all of the information required to configure a process tree.
%%		#network_config
%%
create_worksharing_network(Network_Config,_Process_Delegation) ->
    #network_info{parent_process=?ROOT,children_processes=sets:new(),process_count=1,process_total=1,network_config=Network_Config}.

%%
%%	1) Check mailbox for work requests (requests take precedence over process spawning):
%%		a) While there are still work requests
%%			i)   I have enough work - split
%%			ii)  I don't have enough work - forward request
%%			iii) One of the requests for work is from me - retire
%%	2) Check mail box for new processes and/or retirees
%%		a) If there is a new process notification or suicide notification alter process count accordingly
%%	3) If there aren't enough processes
%%		a)	If I have a critical amount of work then spawn a new process
%%			NB: The spawned process will notify other processes of its creation
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Workstack: The current stack of work available to this process
%%		[#sat_state]
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%
manage_network(Network_Info,Workstack,Process_Delegation) ->
    check_work_exhausted(Network_Info,Process_Delegation,Workstack), %%NB: May not return
    process_tree_util:check_work_complete(Network_Info,Process_Delegation), %% NB: May not return
	Network_Info2 = process_tree_util:check_network_change_messages(Network_Info),
    Conditions = [process_tree_util:under_tree_size(Network_Info2),process_tree_util:over_spawn_threshold(Network_Info2,Workstack)],
    process_tree_util:spawn_if_all_true(Conditions,Network_Info2,Workstack,Process_Delegation).

%%
%%	Returns ok, if it returns at all.
%%	Checks to see if this process has run out of work.  If it has
%%	this process then retires with reason 'no_work'.
%%
check_work_exhausted(Network_Info,Process_Delegation,[]) ->
    process_tree_util:notify_and_retire(Network_Info,Process_Delegation,no_work);

check_work_exhausted(_Network_Info,_Process_Delegation,_WorkStack) ->
    ok.
%%
%%	Will not return.  This function causes the current process to exit.
%%	This will in turn cause each process in this process tree to exit.  It
%%	is therefore important to ensure that all processing and communication
%%	is completed before this function is called.
%%
work_complete(Network_Info,Process_Delegation) ->
    process_tree_util:work_complete(Network_Info,Process_Delegation).
%% Author: Francis Stephens
%% Created: 18 May 2009
%% Description: TODO: Add description to distributed_process_tree
-module(distributed_process_list).

%%
%% Include files
%%

-include("worksharing_records.hrl").
-include("distributed_list_records.hrl").
-include("process_list_records.hrl").
-include("logging_records.hrl").

%%
%% Exported Functions
%%
-export([]).
-compile(export_all).

%%
%%	Macros
%%
-define(LOG_THRESHOLD,5.00000000000000000000e-001).
-define(MESSAGE_LOG,list_messaging_log).

-define(PROCESS_MULT,2).
-define(PROCESS_ADD,0).

%%
%% API Functions
%%

%%
%%	Returns a new #network_info representing the root of a new process tree.
%%
%%	List_Network_Config: A #list_network_config contains all of the information required to configure a process tree.
%%		#list_network_config
%%
create_worksharing_network(Config,Process_Delegation)
  when hd(Config#distributed_network_config.nodes) =:= node() -> %% we must ensure that the first node on the list is the node from which this is being called - or the actual work will be started on the wrong node
	%%disk_log:open([{name,?MESSAGE_LOG},{type,halt},{format,external}]),
    %%log_network_creation(List_Network_Config),
	Nodes = Config#distributed_network_config.nodes,
	List_Network_Configs = Config#distributed_network_config.list_network_configs,
	Preprocessed_Configs = preprocess_network_configs(Nodes,List_Network_Configs),
	Network_Size = get_network_size(Preprocessed_Configs),
	io:format("~n~nNumber of Nodes = ~p~n~nNetwork Size = ~p~n~n",[length(Nodes),Network_Size]),
	spawn_top(tl(Nodes),tl(Preprocessed_Configs),hd(Preprocessed_Configs),Process_Delegation,Network_Size).

%%
%%	Spawn working processes on this, the node of origin
%%	It is important to note that the first process which 
%%	will actually have to work available is only half created here.
%%	It is implicitly created in the #list_network_info record that
%%	is returned by this function call.  It is up to the caller to
%%	do the right thing and actually begin work on it.  All other
%%	processes, including other nodes, are started with empty workstacks
%%	and will hang around until this calling process is willing to share work.
%%
spawn_top([],[],List_Network_Config,Process_Delegation,Network_Size)
  when List_Network_Config#list_network_config.network_size =:= 1 ->
	log_node_creation(List_Network_Config,0),
	#list_network_info{top_process=?TOP_OF_LIST,bottom_process=?BOTTOM_OF_LIST,process_count=Network_Size,network_config=List_Network_Config};

spawn_top(Nodes,List_Network_Configs,List_Network_Config,Process_Delegation,Network_Size)
  when List_Network_Config#list_network_config.network_size =:= 1 ->
	log_node_creation(List_Network_Config,0),
	Self = self(),
	Bottom_Process = erlang:spawn_link(hd(Nodes),fun()-> localise_group_leader(), spawn_rest(tl(Nodes),tl(List_Network_Configs),hd(List_Network_Configs),Process_Delegation,Self,Network_Size,0) end),
	#list_network_info{top_process=?TOP_OF_LIST,bottom_process=Bottom_Process,process_count=Network_Size,network_config=List_Network_Config};

spawn_top(Nodes,List_Network_Configs,List_Network_Config,Process_Delegation,Network_Size) ->
	log_node_creation(List_Network_Config,0),
	Self = self(),
    Bottom_Process = erlang:spawn_link(fun()-> spawn_rest(Nodes,List_Network_Configs,List_Network_Config,Process_Delegation,Self,Network_Size,1) end),
    #list_network_info{top_process=?TOP_OF_LIST,bottom_process=Bottom_Process,process_count=Network_Size,network_config=List_Network_Config}.

%%
%%	Spawn working process on some node
%%
spawn_rest(Nodes,List_Network_Configs,List_Network_Config,Process_Delegation,Top_Process,Network_Size,Spawned_Processes)
  when Spawned_Processes < (List_Network_Config#list_network_config.network_size-1) ->
	io:format("PROCESS CREATED - "),
	log_node_creation(List_Network_Config,Spawned_Processes),
	Self = self(),
	Bottom_Process = erlang:spawn_link(fun()-> spawn_rest(Nodes,List_Network_Configs,List_Network_Config,Process_Delegation,Self,Network_Size,Spawned_Processes+1) end),
	Network_Info = #list_network_info{top_process=Top_Process,bottom_process=Bottom_Process,process_count=Network_Size,network_config=List_Network_Config},
	process_list_util:init_new_process(Network_Info,[],Process_Delegation);

spawn_rest([],[],List_Network_Config,Process_Delegation,Top_Process,Network_Size,Spawned_Processes)
  when Spawned_Processes =:= (List_Network_Config#list_network_config.network_size-1) -> %% When there are no more nodes remaining and we have filled up the processes for this node
	io:format("PROCESS CREATED - "),
	log_node_creation(List_Network_Config,Spawned_Processes),
	Network_Info = #list_network_info{top_process=Top_Process,bottom_process=?BOTTOM_OF_LIST,process_count=Network_Size,network_config=List_Network_Config},
    process_list_util:init_new_process(Network_Info,[],Process_Delegation);

spawn_rest(Nodes,List_Network_Configs,List_Network_Config,Process_Delegation,Top_Process,Network_Size,Spawned_Processes)
  when Spawned_Processes =:= (List_Network_Config#list_network_config.network_size-1) ->
	io:format("PROCESS CREATED - "),
	log_node_creation(List_Network_Config,Spawned_Processes),
	Self = self(),
	Bottom_Process = erlang:spawn_link(hd(Nodes),fun()-> localise_group_leader(), spawn_rest(tl(Nodes),tl(List_Network_Configs),hd(List_Network_Configs),Process_Delegation,Self,Network_Size,0) end),
	Network_Info = #list_network_info{top_process=Top_Process,bottom_process=Bottom_Process,process_count=Network_Size,network_config=List_Network_Config},
	process_list_util:init_new_process(Network_Info,[],Process_Delegation).

localise_group_leader() ->
	Init = whereis(init),
	group_leader(Init,self()),
	io:format("Group Leader Localised~n~n").

get_network_size(List_Network_Configs) ->
	io:format("~n~nlist network configs = ~p~n~n",[List_Network_Configs]),
	Process_Counts = lists:map(fun(List_Network_Config)-> List_Network_Config#list_network_config.network_size end, List_Network_Configs),
	lists:sum(Process_Counts).

preprocess_network_configs(Nodes,List_Network_Configs) ->
	{Scheduler_Counts,_Bad_Nodes} = rpc:multicall(Nodes, erlang, system_info, [schedulers]),
	Process_Requirements = lists:map(fun(Schedulers)-> calc_required_processes(Schedulers) end,Scheduler_Counts),
	Modify_Config = fun({Process_Requirement,List_Network_Config}) ->
			setelement(#list_network_config.network_size, List_Network_Config, Process_Requirement) end,
	lists:map(Modify_Config, lists:zip(Process_Requirements,List_Network_Configs)).

calc_required_processes(Schedulers) ->
	(Schedulers*?PROCESS_MULT)+?PROCESS_ADD.

log_node_creation(List_Network_Config,0) ->
	process_list:log_network_creation(List_Network_Config);

log_node_creation(List_Network_Config,_Spawned_Processes) ->
	ok.

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
	process_list:manage_network(Network_Info, Workstack, Process_Delegation).

%%
%%	Will not return.  This function causes the current process to exit.
%%	This will in turn cause each process in this process tree to exit.  It
%%	is therefore important to ensure that all processing and communication
%%	is completed before this function is called.
%%
work_complete(Network_Info,Process_Delegation,Success_Arg) ->
	process_list_util:work_complete(Network_Info,Process_Delegation,Success_Arg).

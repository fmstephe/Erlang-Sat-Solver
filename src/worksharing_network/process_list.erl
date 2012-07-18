%% Author: Francis Stephens
%% Created: 13 Oct 2008
%% Description: TODO: Add description to process_tree
-module(process_list).

%%
%% Include files
%%

-include("worksharing_records.hrl").
-include("process_list_records.hrl").
-include("logging_records.hrl").

%%
%% Exported Functions
%%
-export([create_worksharing_network/2,manage_network/3,work_complete/2,log_network_creation/1]).

%%
%%	Macros
%%
-define(LOG_THRESHOLD,0.50).
-define(MESSAGE_LOG,list_messaging_log).

%%
%% API Functions
%%

%%
%%	Returns a new #network_info representing the root of a new process tree.
%%
%%	List_Network_Config: A #list_network_config contains all of the information required to configure a process tree.
%%		#list_network_config
%%
create_worksharing_network(List_Network_Config,Process_Delegation) ->
    log_network_creation(List_Network_Config),
	spawn_process_list(List_Network_Config,Process_Delegation).

spawn_process_list(List_Network_Config,Process_Delegation) ->
    Self = self(),
    Bottom_Process = erlang:spawn_link(fun()-> spawn_process_list(List_Network_Config,Process_Delegation,Self,1) end),
    #list_network_info{top_process=?TOP_OF_LIST,bottom_process=Bottom_Process,process_count=List_Network_Config#list_network_config.network_size,network_config=List_Network_Config}.

spawn_process_list(List_Network_Config,Process_Delegation,Top_Process,Spawned_Processes)
  when Spawned_Processes < (List_Network_Config#list_network_config.network_size-1) ->
    Self = self(),
    Bottom_Process = erlang:spawn_link(fun()-> spawn_process_list(List_Network_Config,Process_Delegation,Self,Spawned_Processes+1) end),
    Network_Info = #list_network_info{top_process=Top_Process,bottom_process=Bottom_Process,process_count=List_Network_Config#list_network_config.network_size,network_config=List_Network_Config},
    process_list_util:init_new_process(Network_Info,[],Process_Delegation);

spawn_process_list(List_Network_Config,Process_Delegation,Top_Process,Spawned_Processes)
  when Spawned_Processes =:= (List_Network_Config#list_network_config.network_size-1) ->
    Network_Info = #list_network_info{top_process=Top_Process,bottom_process=?BOTTOM_OF_LIST,process_count=List_Network_Config#list_network_config.network_size,network_config=List_Network_Config},
    process_list_util:init_new_process(Network_Info,[],Process_Delegation).

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
    process_list_util:check_work_complete(Network_Info,Process_Delegation), %% NB: May not return
	Network_Workstack = process_list_util:request_work(Network_Info,Workstack,Process_Delegation,?INITIAL_BACKOFF), %% NB: May not return
	Network_Info2 = Network_Workstack#network_workstack.network_info,
	Workstack2 = Network_Workstack#network_workstack.workstack,
	Workstack3 = process_list_util:check_work_requests(Network_Info2,Workstack2),
	Network_Info3 = process_list_util:check_network_change_messages(Network_Info2),
    Return_Network_Workstack = #network_workstack{network_info=Network_Info3,workstack=Workstack3},
    log_network_management(Network_Info,Workstack,Return_Network_Workstack),
	Return_Network_Workstack.

%%
%%	Will not return.  This function causes the current process to exit.
%%	This will in turn cause each process in this process tree to exit.  It
%%	is therefore important to ensure that all processing and communication
%%	is completed before this function is called.
%%
work_complete(Network_Info,Process_Delegation) ->
    process_list_util:work_complete(Network_Info,Process_Delegation).

log_network_creation(List_Network_Config) ->
	Log_Name = util:value_or_default(List_Network_Config#list_network_config.log_name,?MESSAGE_LOG),
	disk_log:open([{name,Log_Name},{type,halt},{format,external}]),
	Log_Entry = lists:flatten(format_log_entry(List_Network_Config)),
	disk_log:balog(Log_Name,Log_Entry).

log_network_management(Network_Info,Workstack,Network_Workstack) ->
	Log_Name = util:value_or_default((Network_Info#list_network_info.network_config)#list_network_config.log_name,?MESSAGE_LOG),
    Float = random:uniform(),
    case Float > ?LOG_THRESHOLD of
        true
        	->	Log_Entry = lists:flatten([format_pre(Network_Info,Workstack),format_post(Network_Workstack)]),
    			disk_log:balog(Log_Name,Log_Entry);
        false
        	->	[]
    end.

format_pre(Network_Info,Workstack) ->
    format_log_entry("PRE",Network_Info,Workstack).

format_post(Network_Workstack) ->
    Network_Info = Network_Workstack#network_workstack.network_info,
    Workstack = Network_Workstack#network_workstack.workstack,
    format_log_entry("POST",Network_Info,Workstack).

format_log_entry(List_Network_Config) ->
	[[?HARD_RETURN],
     "************************************************************",[?HARD_RETURN],
     "Network Size = ",util:format_int(List_Network_Config#list_network_config.network_size),[?HARD_RETURN],
     "Work Sharing Threshold = ",util:format_int(List_Network_Config#list_network_config.work_sharing_threshold),[?HARD_RETURN],
     "************************************************************",[?HARD_RETURN,?HARD_RETURN]
    ].

format_log_entry(Title,Network_Info,Workstack) ->
    [Title,[?HARD_RETURN],
	 util:get_time_stamp(), [?HARD_RETURN],
	 "Node ", erlang:atom_to_list(erlang:node()),[?HARD_RETURN],
     "I ",erlang:pid_to_list(self()),[?HARD_RETURN],
     "Number of running processes = ", util:format_int(Network_Info#list_network_info.process_count), [?HARD_RETURN],
     "Is Top Of List = ", erlang:atom_to_list(Network_Info#list_network_info.top_process =:= ?TOP_OF_LIST), [?HARD_RETURN],
     "Is Bottom Of List= ", erlang:atom_to_list(Network_Info#list_network_info.bottom_process =:= ?BOTTOM_OF_LIST), [?HARD_RETURN],
	 "Work available = ", util:format_int(length(Workstack)),[?HARD_RETURN,?HARD_RETURN]].
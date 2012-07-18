%% Author: Francis Stephens
%% Created: 13 Oct 2008
%% Description: TODO: Add description to process_tree
-module(process_tree).

%%
%% Include files
%%

-include("worksharing_records.hrl").
-include("process_tree_records.hrl").
-include("logging_records.hrl").

%%
%% Exported Functions
%%
-export([create_worksharing_network/2,manage_network/3,work_complete/2]).

%%
%%	Macros
%%
-define(LOG_THRESHOLD,0.50).
-define(MESSAGE_LOG,tree_messaging_log).

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
    disk_log:open([{name,?MESSAGE_LOG},{type,halt},{format,external}]),
    log_network_creation(Network_Config),
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
    process_tree_util:check_work_complete(Network_Info,Process_Delegation), %% NB: May not return
    Network_Workstack = process_tree_util:request_work(Network_Info,Workstack,Process_Delegation), %% NB: May not return
	Network_Info2 = Network_Workstack#network_workstack.network_info,
	Workstack2 = Network_Workstack#network_workstack.workstack,
	Workstack3 = process_tree_util:check_work_requests(Network_Info2,Workstack2),
	Network_Info3 = process_tree_util:check_network_change_messages(Network_Info2),
    Conditions = [process_tree_util:under_tree_size(Network_Info3),process_tree_util:over_spawn_threshold(Network_Info3,Workstack3)],
    Return_Network_Workstack = process_tree_util:spawn_if_all_true(Conditions,Network_Info3,Workstack3,Process_Delegation),
    log_network_management(Network_Info,Workstack,Return_Network_Workstack),
    Return_Network_Workstack.

%%
%%	Will not return.  This function causes the current process to exit.
%%	This will in turn cause each process in this process tree to exit.  It
%%	is therefore important to ensure that all processing and communication
%%	is completed before this function is called.
%%
work_complete(Network_Info,Process_Delegation) ->
    process_tree_util:work_complete(Network_Info,Process_Delegation).

log_network_creation(Network_Config) ->
    Log_Entry = lists:flatten(format_log_entry(Network_Config)),
    disk_log:balog(?MESSAGE_LOG,Log_Entry).

log_network_management(Network_Info,Workstack,Network_Workstack) ->
    Float = random:uniform(),
    case Float > ?LOG_THRESHOLD of
        true
        	->	Log_Entry = lists:flatten([format_pre(Network_Info,Workstack),format_post(Network_Workstack)]),
    			disk_log:balog(?MESSAGE_LOG,Log_Entry);
        false
        	->	[]
    end.

format_pre(Network_Info,Workstack) ->
    format_log_entry("PRE",Network_Info,Workstack).

format_post(Network_Workstack) ->
    Network_Info = Network_Workstack#network_workstack.network_info,
    Workstack = Network_Workstack#network_workstack.workstack,
    format_log_entry("POST",Network_Info,Workstack).

format_log_entry(Network_Config) ->
	[[?HARD_RETURN],
     "************************************************************",[?HARD_RETURN],
     "Max Tree Size = ",util:format_int(Network_Config#network_config.max_tree_size),[?HARD_RETURN],
     "Process Spawn Threshold = ",util:format_int(Network_Config#network_config.process_spawn_threshold),[?HARD_RETURN],
     "Work Sharing Threshold = ",util:format_int(Network_Config#network_config.work_sharing_threshold),[?HARD_RETURN],
     "************************************************************",[?HARD_RETURN,?HARD_RETURN]
    ].

format_log_entry(Title,Network_Info,Workstack) ->
    [is_root_process(Network_Info),
    Title,[?HARD_RETURN],
    "Number of running processes = ", util:format_int(Network_Info#network_info.process_count), [?HARD_RETURN],
    "Total number of processes = ", util:format_int(Network_Info#network_info.process_total), [?HARD_RETURN],
    "Number of child processes = ", util:format_int(sets:size(Network_Info#network_info.children_processes)), [?HARD_RETURN],
	"Work available = ", util:format_int(length(Workstack)),[?HARD_RETURN,?HARD_RETURN]].

is_root_process(Network_Info) ->
    case Network_Info#network_info.parent_process of
        ?ROOT
			->	["Root Process",[?HARD_RETURN]];
        _Anything_Else
			->	[]
    end.
%% Author: Francis Stephens
%% Created: 24 Feb 2009
%% Description: TODO: Add description to process_tree_util
-module(process_list_util).

%%
%% Include files
%%

-include("worksharing_records.hrl").
-include("process_list_records.hrl").

%%
%% Exported Functions
%%
-export([get_default_config/0,
         work_complete/3,check_work_complete/2,
         request_work/4,check_work_requests/2,
         broadcast_message/3,check_network_change_messages/1,
         notify_and_retire/4,
         init_new_process/3]).

%%
%% API Functions
%%

%%
%%	Returns a default network configuration.
%%	This configuration is under ongoing optimisation.
%%
get_default_config() ->
    #list_network_config{network_size=8,
                    	 work_sharing_threshold=2,
                    	 work_splitting_fun=fun(List)->util:split_alternate(List)end}.

%%
%%	Will not return.  This function causes the current process to exit.
%%	This will in turn cause each process in this process tree to exit.  It
%%	is therefore important to ensure that all processing and communication
%%	is completed before this function is called.
%%
work_complete(_Network_Info,Process_Delegation,Success_Arg) ->
	Module = Process_Delegation#process_delegation.module,
    Work_Complete_Fun = Process_Delegation#process_delegation.work_complete_fun,
    Work_Complete_Args = Process_Delegation#process_delegation.work_complete_args,
    apply(Module,Work_Complete_Fun,[Success_Arg|Work_Complete_Args]),
    exit(terminating_network).

%%
%%	Returns ok, or does not return at all if in fact work is complete.
%%
%%	There are two circumstances under which we can determine that work has 
%%	completed:
%%	1-	If this process's Workstack is empty and it has no children and it is the 
%%		root (i.e. this process tree is running a single process) then work has
%%		completed.
%%	2-	If there is a #work_complete message in this process's mailbox then
%%		work has completed.
%%
%%	If it is determined that work has completed then this function never returns
%%	and this process will retire in the usual manner.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%
check_work_complete(Network_Info,Process_Delegation) ->
    receive 
		Message when is_record(Message#list_network_message.message,work_complete)
			->	notify_and_retire(Network_Info,Process_Delegation,work_complete,0)
		after 0
			->	ok
    end.

%%
%%	Returns a new #network_workstack containing the current
%%	state of the network and work for this process to do.
%%	If the caller to this function has a non-empty workstack then
%%	the parameters provided are returned directly (the caller has enough work)
%%	otherwise a #work_request is broadcast across the network.
%%	The requesting process must then wait for a response from some other process
%%	while ensuring that it continues to process and propogate network messages in
%%	the meantime.  Be aware that calls to this function may never return in the 
%%	lifetime of the calling process.  If a #work_request returns to the calling
%%	process it will retire itself and become a non-operational process which serves only
%%	to forward messages passed to it.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Workstack: The current stack of work available to this process, if empty we will request some work
%%		[#sat_state]
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%	
request_work(Network_Info,[],Process_Delegation,Backoff) ->
    Work_Request = #work_request{requester_pid=self()},
    broadcast_message(Network_Info,Work_Request,self()),
    await_work_request(Network_Info,Process_Delegation,Backoff);

request_work(Network_Info,Workstack,_Process_Delegation,_Backoff) ->
    #network_workstack{network_info=Network_Info,workstack=Workstack}.

%%
%%	Returns a new #workstack which is a subset of the #workstack
%%	provided as parts may have been shared with other processes in
%%	the tree.
%%
%%	Because this function communicates with other processes, in particular
%%	sharing its workstack with the, it has important side-effects.
%%
%%	Calling this function may cause the calling process to commit suicide 
%%	if it finds a work request that it originally made has been passed back 
%%	to it.
%%
%%	1) Check mailbox for work requests (requests take precedence over process spawning):
%%		a) While there are still work requests
%%			i)   I have enough work - split
%%			ii)  I don't have enough work - deny
%%			iii) One of the requests for work is from me - commit suicide
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Workstack: The current stack of work available to this process
%%		[#sat_state]
%%
check_work_requests(Network_Info,Workstack) ->
	receive
		Message when is_record(Message#list_network_message.message,work_request)
			->	Workstack2 = process_work_request(Network_Info,Workstack,Message),
            	check_work_requests(Network_Info,Workstack2)
		after 0
			->	Workstack
    end.

await_work_request(Network_Info,Process_Delegation,Backoff) ->
    receive
		Message when is_record(Message,work_response)
            ->	#network_workstack{network_info=Network_Info,workstack=Message#work_response.workstack};
		Message when is_record(Message#list_network_message.message,work_request)
			->	is_poison_work_request(Network_Info,Message,Process_Delegation,Backoff);
		Message when is_record(Message#list_network_message.message,network_change)
            ->	New_Network_Info = process_network_change(Network_Info,Message),
            	await_work_request(New_Network_Info,Process_Delegation,Backoff)
    end.

is_poison_work_request(Network_Info,Work_Request_Message,Process_Delegation,Backoff) ->
	My_Pid = self(),
	Work_Request = Work_Request_Message#list_network_message.message,
	case Work_Request#work_request.requester_pid =:= My_Pid of
		true
			->	notify_and_retire(Network_Info,Process_Delegation,no_work,Backoff);
		false
			->	forward_network_message(Network_Info,Work_Request_Message),
				await_work_request(Network_Info,Process_Delegation,Backoff)
	end.

%%
%%	Helper function will take any message record and broadcast it across the network.
%%	In order for a message to be broadcast across the network we must initialise the
%%	#network_message properly. In particular:
%%		1) Visited_Nodes must contain this process only, and
%%		2) Forwarding_Stack must be empty
%%	We can then safely pass this #network_message to the forward_network_message/2 function
%%	for normal message propogation.
%%
%%	NB: If this process is the root process with no children then this message will quietly not
%%		be sent.
%%
%%	Message: The message to be broadcast
%%		Term()
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Final_Destination: Indicates what to do with this message when it has been passed to
%%					   every process in the tree.  If a Pid() then the message will be forwarded
%%					   to that process, otherwise the message will be abandoned.
%%		Term() | cancel_message | Pid()
%%
broadcast_message(Network_Info,Message,Final_Destination) ->
	Network_Message = #list_network_message{top_process=Network_Info#list_network_info.top_process,
									   		bottom_process=Network_Info#list_network_info.bottom_process,
									   		final_destination=Final_Destination,
									   		message=Message,
                                       		time_sent=time()},
	forward_network_message(Network_Info,Network_Message).

%%
%%	Returns a new #network_info with an updated network process count.
%%	The network process count acts as a 'to the best of my knowledge'
%%	counter for the number processes in the process tree.  It is not
%%	guaranteed to be precisely accurate as processes may be retiring
%%	and spawning and we must wait for these messages to propogate
%%	through the tree.  However, it is guaranteed that if the process_count
%%	for any process reaches 0 then every process in the tree has retired.
%%	This guarantee is provided by the 'visibility' properties of persistent
%%	messages such as #network_change messages (see 'Network Communication Guarantees'
%%	for more details) as well as the guarantee that when a process is created no 
%%	retirement message will be sent out by either the new process or its parent
%%	before the #network_change notification has been sent out for the birth of
%%	the new process.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%
check_network_change_messages(Network_Info) ->
	receive
		Message when is_record(Message#list_network_message.message,network_change)
			->	New_Network_Info = process_network_change(Network_Info,Message),
				check_network_change_messages(New_Network_Info)
		after 0
			->	Network_Info
    end.

%%
%%	Returns an updated #network_info where if the process change message indicates that the number of processes
%%	in the network has increased or decreased the process_count in the #network_info is incremented or decremented
%%	appropriately.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%
process_network_change(Network_Info,Node_Change_Message) ->
	forward_network_message(Network_Info,Node_Change_Message),
	Notification = (Node_Change_Message#list_network_message.message)#network_change.notification,
	Notifier_Pid = (Node_Change_Message#list_network_message.message)#network_change.notifier_pid,
    case Notification of
        increase
        	->	setelement(#list_network_info.process_count,Network_Info,Network_Info#list_network_info.process_count+1);
    	decrease
			->	setelement(#list_network_info.process_count,Network_Info,Network_Info#list_network_info.process_count-1)
    end.

%%
%%	Notifies the network that this process is retiring (i.e. there is one less process available
%%	to the network) then retires.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%	Reason: The reason why this process has retired
%%		work_complete | no_work
%%
notify_and_retire(Network_Info,Process_Delegation,Reason,Backoff) ->
	broadcast_message(Network_Info,#network_change{notification=decrease,notifier_pid=self()},cancel_message),
	New_Network_Info = setelement(#list_network_info.process_count,Network_Info,Network_Info#list_network_info.process_count-1),
    erlang:send_after(Backoff,self(),?WAKE_UP),
	retire(New_Network_Info,Process_Delegation,Reason,Backoff).

%%
%%	This infinitely looping function turns a process into an inactive process which serves only
%%	to forward received messages to the rest of the network.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%	Reason: The reason this process has retired
%%		work_complete | no_work
%%

%%	If the process_count for Network_Info is zero then every process in the network is retired.
%%	If this process retired because there was no work then We respond to this by calling the 
%%	no_work_fun function in Process_Delegation which should handle this.
retire(Network_Info,Process_Delegation,no_work,_Backoff) when Network_Info#list_network_info.process_count =:= 0 ->
    Module = Process_Delegation#process_delegation.module,
    Delegation_Fun = Process_Delegation#process_delegation.no_work_fun,
    No_Work_Args = Process_Delegation#process_delegation.no_work_args,
    apply(Module,Delegation_Fun,No_Work_Args),
    exit(terminating_network);

retire(Network_Info,_Process_Delegation,work_complete,_Backoff) when Network_Info#list_network_info.process_count =:= 0 ->
    exit(terminating_network);

%%	In this second version of retire/2 we just loop endlessly forwarding messages as usual
%%	until we detect that Network_Info's process_count is zero (see above)
retire(Network_Info,Process_Delegation,Reason,Backoff) ->
    receive
		Message when is_record(Message#list_network_message.message,network_change)
            ->	New_Network_Info = process_network_change(Network_Info,Message),
            	retire(New_Network_Info,Process_Delegation,Reason,Backoff);
		Network_Message when is_record(Network_Message,list_network_message)
			->	forward_network_message(Network_Info,Network_Message),
				retire(Network_Info,Process_Delegation,Reason,Backoff);
    	?WAKE_UP
			->	broadcast_message(Network_Info,#network_change{notification=increase,notifier_pid=self()},cancel_message),
				Awake_Network_Info = erlang:setelement(#list_network_info.process_count,Network_Info,Network_Info#list_network_info.process_count+1),
				request_work(Awake_Network_Info,[],Process_Delegation,(Backoff*2))
    end.

%%
%%	Returns a workstack ([#sat_state]) which is either the same as the one provided or half of it
%%	split in some way.  A work request is processed by either sharing work (by splitting the workstack)
%%	or forwarding the request on to this process's children and parent processes.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Workstack: The current stack of work available to this process
%%		[#sat_state]
%%	Work_Request: Describes a request for work including where its been where it is headed and who made the request
%%		#work_request
%%
process_work_request(Network_Info,Workstack,Work_Request_Message) ->
    Work_Splitting_Threshold = (Network_Info#list_network_info.network_config)#list_network_config.work_sharing_threshold,
    case length(Workstack) > Work_Splitting_Threshold of
		true
			->	Workstack2 = share_work(Network_Info,Workstack,Work_Request_Message),
				Workstack2;
		false
			->	forward_network_message(Network_Info,Work_Request_Message),
				Workstack
	end.

%%
%%	Returns a workstack which will be one of two halves of the workstack provided.
%%	As a side-effect the other half of the split workstack is sent back to the process
%%	who initiated the #work_request.
%%
%%	Workstack: The workstack to be shared
%%		[#sat_state]
%%	Work_Request: Describes a request for work including where its been where it is headed and who made the request
%%		#work_request
%%
share_work(Network_Info,Workstack,Work_Request_Message) ->
	Work_Splitting_Fun = (Network_Info#list_network_info.network_config)#list_network_config.work_splitting_fun,
	Work_Request = Work_Request_Message#list_network_message.message,
	{My_Stack,Your_Stack} = Work_Splitting_Fun(Workstack),
    Work_Response = #work_response{workstack=Your_Stack,responder_pid=self()},
    Work_Request#work_request.requester_pid ! Work_Response,
    My_Stack.

%%
%%	Returns ok.  The #network_message provided is forwarded on through the network list by the following process.
%%
%%	1: If the message has been propogated through the entire network list it is forwarded to the final destination or discarded
%%	2: If the message has reached the top of the list it is propogated down the list
%%	3: If the message has reached the bottom of the list it is propogated up the list
%%	4: If the message was sent from a lower process in the list it is propogated down the list
%%	5: If the message was sent from a higher process in the list it is propogated up the list
%%	6: If the message has only just been created it is propogated up the list
%%
%%	Network_Info: Contains information locating this process in the process list.
%%		#list_network_info
%%	Network_Message: Describes a message for the network including where it has been, where it is headed and where its final destiation is
%%		#list_network_message
%%

%% When both the top_process and bottom_process are at the end, the message is forwarded to the final destination if it is a Pid otherwise the message is discarded
forward_network_message(_Network_Info,Network_Message) 
  when (Network_Message#list_network_message.top_process =:= ?TOP_OF_LIST) 
   and (Network_Message#list_network_message.bottom_process =:= ?BOTTOM_OF_LIST) ->
    Final_Destination = Network_Message#list_network_message.final_destination,
    case is_pid(Final_Destination) of
        true
        	->	Final_Destination ! Network_Message,
                ok;
        false
  			->	ok
    end;

%% When the top_process is 'top_of_list' then messages are forwarded down the list only
forward_network_message(Network_Info,Network_Message)
  when (Network_Message#list_network_message.top_process =:= ?TOP_OF_LIST) ->
    Bottom_Process = Network_Info#list_network_info.bottom_process,
    New_Network_Message = erlang:setelement(#list_network_message.bottom_process,Network_Message,Bottom_Process),
    case Bottom_Process =:= ?BOTTOM_OF_LIST of
        true
        	->	%% Reforwarding the message with the new bottom_process ensures correct handling of exhausted 
            	forward_network_message(Network_Info,New_Network_Message);
        false
    		->	Bottom_Process ! New_Network_Message,
    			ok
	end;

%% When the bottom_process is 'bottom_of_list' then messages are forwarded up the list only
forward_network_message(Network_Info,Network_Message) 
  when (Network_Message#list_network_message.bottom_process =:= ?BOTTOM_OF_LIST) ->
    Top_Process = Network_Info#list_network_info.top_process,
	New_Network_Message = erlang:setelement(#list_network_message.top_process,Network_Message,Top_Process),
    case Top_Process =:= ?TOP_OF_LIST of
        true
        	->	%% Reforwarding the message with the new top_process ensures correct handling of exhausted messages
            	forward_network_message(Network_Info,New_Network_Message);
        false
        	->	Top_Process ! New_Network_Message,
    			ok
	end;

%% When the message's top_process is the same as this process then we replace the message's top_process and forward it to the message's bottom_process
forward_network_message(Network_Info,Network_Message) 
  when Network_Message#list_network_message.top_process =:= self() ->
    Top_Process = Network_Info#list_network_info.top_process,
	New_Network_Message = erlang:setelement(#list_network_message.top_process,Network_Message,Top_Process),
    New_Network_Message#list_network_message.bottom_process ! New_Network_Message,
    ok;

%% When the message's bottom_process is the same as this process then we replace the message's bottom_process and forward it to the message's top_process
forward_network_message(Network_Info,Network_Message) 
  when Network_Message#list_network_message.bottom_process =:= self() ->
	Bottom_Process = Network_Info#list_network_info.bottom_process,
	New_Network_Message = erlang:setelement(#list_network_message.bottom_process,Network_Message,Bottom_Process),
    New_Network_Message#list_network_message.top_process ! New_Network_Message,
    ok;

%% When the message's top and bottom processes don't match this process's Pid we know this message has just been created and we forward it to the message's top_process 
forward_network_message(_Network_Info,Network_Message) ->
	Network_Message#list_network_message.top_process ! Network_Message,
    ok.

%%
%%	This function waits for a notification of its birth then the work is delegated
%%	on to the function etc. provided by Process_Delegation.
%%
%%	Network_Info: Contains information locating this new process in the process tree
%%		#network_info
%%	Workstack: The stack of work to be delegated to this new process
%%		[#sat_state]
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%
init_new_process(Network_Info,Workstack,Process_Delegation) ->
    Module = Process_Delegation#process_delegation.module,
    Delegation_Fun = Process_Delegation#process_delegation.delegation_fun,
    Additional_Args = Process_Delegation#process_delegation.delegation_args,
    apply(Module,Delegation_Fun,[Network_Info,Workstack|Additional_Args]).
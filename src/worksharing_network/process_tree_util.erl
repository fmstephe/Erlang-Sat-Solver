%% Author: Francis Stephens
%% Created: 24 Feb 2009
%% Description: TODO: Add description to process_tree_util
-module(process_tree_util).

%%
%% Include files
%%

-include("worksharing_records.hrl").
-include("process_tree_records.hrl").

%%
%% Exported Functions
%%
-export([get_default_config/0,
         work_complete/2,check_work_complete/2,
         request_work/3,check_work_requests/2,
         broadcast_message/3,check_network_change_messages/1,
         spawn_if_all_true/4,under_tree_size/1,over_spawn_threshold/2,spawn_child/3,
         init_child_process/3,
         notify_and_retire/3]).

%%
%% API Functions
%%

%%
%%	Returns a default network configuration.
%%	This configuration is under ongoing optimisation.
%%
get_default_config() ->
    #network_config{max_tree_size=4,
                    process_spawn_threshold=1,
                    work_sharing_threshold=5,
                    work_splitting_fun=fun(List)->util:split_alternate(List)end}.

%%
%%	Will not return.  This function causes the current process to exit.
%%	This will in turn cause each process in this process tree to exit.  It
%%	is therefore important to ensure that all processing and communication
%%	is completed before this function is called.
%%
work_complete(_Network_Info,_Process_Delegation) ->
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
		Message when is_record(Message#network_message.message,work_complete)
			->	notify_and_retire(Network_Info,Process_Delegation,work_complete)
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
request_work(Network_Info,[],Process_Delegation) ->
    Work_Request = #work_request{requester_pid=self()},
    broadcast_message(Network_Info,Work_Request,self()),
    await_work_request(Network_Info,Process_Delegation);

request_work(Network_Info,Workstack,_Process_Delegation) ->
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
		Message when is_record(Message#network_message.message,work_request)
			->	Workstack2 = process_work_request(Network_Info,Workstack,Message),
            	check_work_requests(Network_Info,Workstack2)
		after 0
			->	Workstack
    end.

await_work_request(Network_Info,Process_Delegation) ->
    receive
		Message when is_record(Message,work_response)
          	->	#network_workstack{network_info=Network_Info,workstack=Message#work_response.workstack};
		Message when is_record(Message#network_message.message,work_request)
			->	is_poison_work_request(Network_Info,Message,Process_Delegation);
		Message when is_record(Message#network_message.message,network_change)
            ->	New_Network_Info = process_network_change(Network_Info,Message),
            	await_work_request(New_Network_Info,Process_Delegation)
    end.

is_poison_work_request(Network_Info,Work_Request_Message,Process_Delegation) ->
	My_Pid = self(),
	Work_Request = Work_Request_Message#network_message.message,
	case Work_Request#work_request.requester_pid =:= My_Pid of
		true
			->	notify_and_retire(Network_Info,Process_Delegation,no_work);
		false
			->	forward_network_message(Network_Info,Work_Request_Message),
				await_work_request(Network_Info,Process_Delegation)
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
    Visited_Nodes = sets:add_element(self(),sets:new()),
	Forwarding_Stack = [],
	Network_Message = #network_message{visited_processes=Visited_Nodes,
									   forwarding_stack=Forwarding_Stack,
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
		Message when is_record(Message#network_message.message,network_change)
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
	Notification = (Node_Change_Message#network_message.message)#network_change.notification,
    case Notification of
        increase
        	->	Network_Info2 = setelement(#network_info.process_count,Network_Info,Network_Info#network_info.process_count+1),
                setelement(#network_info.process_total,Network_Info2,Network_Info#network_info.process_total+1);
    	decrease
			->	setelement(#network_info.process_count,Network_Info,Network_Info#network_info.process_count-1)
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
notify_and_retire(Network_Info,Process_Delegation,Reason) ->
	broadcast_message(Network_Info,#network_change{notification=decrease,notifier_pid=self()},cancel_message),
	New_Network_Info = setelement(#network_info.process_count,Network_Info,Network_Info#network_info.process_count-1),
	retire(New_Network_Info,Process_Delegation,Reason).

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
retire(Network_Info,Process_Delegation,no_work) when Network_Info#network_info.process_count =:= 0 ->
    Module = Process_Delegation#process_delegation.module,
    Delegation_Fun = Process_Delegation#process_delegation.no_work_fun,
    No_Work_Args = Process_Delegation#process_delegation.no_work_args,
    apply(Module,Delegation_Fun,No_Work_Args),
    exit(terminating_network);

retire(Network_Info,_Process_Delegation,work_complete) when Network_Info#network_info.process_count =:= 0 ->
    exit(terminating_network);

%%	In this second version of retire/2 we just loop endlessly forwarding messages as usual
%%	until we detect that Network_Info's process_count is zero (see above)
retire(Network_Info,Process_Delegation,Reason) ->
    receive
		Message when is_record(Message#network_message.message,network_change)
            ->	New_Network_Info = process_network_change(Network_Info,Message),
            	retire(New_Network_Info,Process_Delegation,Reason);
		Network_Message when is_record(Network_Message,network_message)
			->	forward_network_message(Network_Info,Network_Message),
				retire(Network_Info,Process_Delegation,Reason)
    end.



%%	If the process_count for Network_Info is zero then every process in the network is retired.
%%	If this process retired because there was no work then We respond to this by calling the 
%%	no_work_fun function in Process_Delegation which should handle this.
hibernate(Network_Info,Process_Delegation,no_work) when Network_Info#network_info.process_count =:= 0 ->
    Module = Process_Delegation#process_delegation.module,
    Delegation_Fun = Process_Delegation#process_delegation.no_work_fun,
    No_Work_Args = Process_Delegation#process_delegation.no_work_args,
    apply(Module,Delegation_Fun,No_Work_Args),
    exit(terminating_network);

hibernate(Network_Info,_Process_Delegation,work_complete) when Network_Info#network_info.process_count =:= 0 ->
    exit(terminating_network);

%%	In this second version of retire/2 we just loop endlessly forwarding messages as usual
%%	until we detect that Network_Info's process_count is zero (see above)
hibernate(Network_Info,Process_Delegation,Reason) ->
    receive
		Message when is_record(Message#network_message.message,network_change)
            ->	New_Network_Info = process_network_change(Network_Info,Message),
            	hibernate(New_Network_Info,Process_Delegation,Reason);
		Network_Message when is_record(Network_Message,network_message)
			->	forward_network_message(Network_Info,Network_Message),
				hibernate(Network_Info,Process_Delegation,Reason)
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
    Work_Splitting_Threshold = (Network_Info#network_info.network_config)#network_config.work_sharing_threshold,
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
	Work_Splitting_Fun = (Network_Info#network_info.network_config)#network_config.work_splitting_fun,
	Work_Request = Work_Request_Message#network_message.message,
	{My_Stack,Your_Stack} = Work_Splitting_Fun(Workstack),
    Work_Response = #work_response{workstack=Your_Stack,responder_pid=self()},
    Work_Request#work_request.requester_pid ! Work_Response,
    My_Stack.

%%
%%	Returns ok.  The #network_message provided is forwarded on through the network by the following process.
%%	
%%	The #network_message has the following processes pushed onto its Forwarding_Stack in this order
%%		1: The parent of this process, if it is not a member of Visited_Nodes
%%		2: Each of the children of this process, provided they are not members of Visited_Nodes
%%	Each process that is pushed onto Forwarding_Stack is also added to Visited_Nodes.  Where any child process
%%	was not pushed onto Forwarding_Stack because it was in Visited_Nodes that process is removed from 
%%	Visited_Nodes because it will never be a candidate for forwarding again.
%%
%%	Network_Info: Contains information locating this process in the process tree.
%%		#network_info
%%	Network_Message: Describes a message for the network including where it has been, where it is headed and where its final destiation is
%%		#network_message
%%
forward_network_message(Network_Info,Network_Message) ->
    Parent_Process = Network_Info#network_info.parent_process,
	Children_Processes = Network_Info#network_info.children_processes,
	New_Network_Message = prepare_message_for_propogation(Network_Message,Parent_Process,Children_Processes),
    [Next_Node|_Forwarding_Stack] = New_Network_Message#network_message.forwarding_stack,
	case is_pid(Next_Node) of
		true
			->	Next_Node ! New_Network_Message;
		false
			->	message_cancelled
	end.

%%
%%	The #work_request provided is forwarded on through the network
%%	by the following process.
%%	The #work_request has the following processes pushed onto its forwarding stack in this order.
%%		1: The parent of this process, if it is not a member of Visited_Nodes
%%		2: Each of the children of this process, provided they are not members of Visited_Nodes
%%	Each process that is pushed onto the new forwarding_stack is also added the new visited_processes set.
%%	Where any child process was excluded from the forwarding stack because it was already in visited_processes
%%	that process is removed from the new visited_processes because it will never be a forwarding candidate again.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Work_Request: Describes a request for work including where its been where it is headed and who made the request
%%		#work_request
%%

%%	This first version of this function observes that if the network message is found by the root we can replace
%%	the parent_process parameter with the #network_message's final_destination and ensure that final_destination will
%%	be pushed onto the forwarding_stack appropriately.
prepare_message_for_propogation(Network_Message,?ROOT,Children_Processes) ->
    Visited_Nodes = Network_Message#network_message.visited_processes,
	%% By taking the tail of the forwarding stack we remove the Pid reference to this process process
    Forwarding_Stack = util:tail_or_empty(Network_Message#network_message.forwarding_stack),
	Visitable_Children = sets:subtract(Children_Processes,Visited_Nodes),
	Visited_Minus_Children = sets:subtract(Visited_Nodes,Children_Processes),
	New_Visited_Nodes = sets:union(Visitable_Children,Visited_Minus_Children),
	New_Forwarding_Stack = sets:to_list(Visitable_Children) ++ [Network_Message#network_message.final_destination|Forwarding_Stack],
	#network_message{visited_processes=New_Visited_Nodes,
					 forwarding_stack=New_Forwarding_Stack,
					 final_destination=Network_Message#network_message.final_destination,
					 message=Network_Message#network_message.message,
                     time_sent=Network_Message#network_message.time_sent};
    
prepare_message_for_propogation(Network_Message,Parent_Process,Children_Processes) ->
    Visited_Nodes = Network_Message#network_message.visited_processes,
	%% By taking the tail of the forwarding stack we remove the Pid reference to this process process
    Forwarding_Stack = util:tail_or_empty(Network_Message#network_message.forwarding_stack),
    Reachable_Nodes = sets:add_element(Parent_Process,Children_Processes),
    My_Visited_Nodes = sets:intersection(Reachable_Nodes,Visited_Nodes),
	Visitable_Nodes = sets:subtract(Reachable_Nodes,My_Visited_Nodes),
	Visitable_Children = sets:del_element(Parent_Process,Visitable_Nodes),
	Visited_Minus_Children = sets:subtract(Visited_Nodes,Children_Processes),
	New_Visited_Nodes = sets:union(Visitable_Nodes,Visited_Minus_Children),
	New_Forwarding_Stack = create_forwarding_stack(Forwarding_Stack,Parent_Process,Visitable_Children,Visited_Nodes),
	#network_message{visited_processes=New_Visited_Nodes,
					 forwarding_stack=New_Forwarding_Stack,
					 final_destination=Network_Message#network_message.final_destination,
					 message=Network_Message#network_message.message,
                     time_sent=Network_Message#network_message.time_sent}.

%%
%%	Returns a new forwarding stack with Visitable_Children on top and parent_process directly after
%%	them provided it has not already been visited.
%%
%%	Forwarding_Stack: The existing stack of processes to which this message will be forwarded
%%		[Pid | Term()]
%%	parent_process: The parent process of this process
%%		Pid()
%%	Visitable_Children: The children of this process to whom this message may be forwarded
%%		Set()
%%
create_forwarding_stack(Forwarding_Stack,Parent_Process,Visitable_Children,Visited_Nodes) ->
	case sets:is_element(Parent_Process,Visited_Nodes) of
		true
			->	sets:to_list(Visitable_Children) ++ Forwarding_Stack;
		false
			->	sets:to_list(Visitable_Children) ++ [Parent_Process|Forwarding_Stack]
	end.

%%
%%	Returns true if the current process count at this process is less than the maximum tree size
%%	set in the network config.  Returns false otherwise.
%%
%%	Network_Info: #network_info containing the current process count and network config
%%		#network_info
%%
under_tree_size(Network_Info) ->
    Process_Count = Network_Info#network_info.process_count,
    Max_Tree_Size = (Network_Info#network_info.network_config)#network_config.max_tree_size,
    Process_Count < Max_Tree_Size.

%%
%%	Returns true if the amount of work in Workstack is greater than the spawn threshold set
%%	in the network config.  Returns false otherwise.
%%
%%	Network_Info: #network_info containing the network config
%%		#network_info
%%	Workstack: List of #sat_state defining the work allotted to this process
%%		[#sat_state]
%%
over_spawn_threshold(Network_Info,Workstack) ->
	Process_Spawn_Threshold = (Network_Info#network_info.network_config)#network_config.process_spawn_threshold,
	length(Workstack) > Process_Spawn_Threshold.

%%
%%	Returns a #network_workstack representing the valid network info and workstack for this process.
%%	If every element in Conditions is the atom 'true' then a new process will be spawned.
%%
spawn_if_all_true(Conditions,Network_Info,Workstack,Process_Delegation) ->
    case ternary_logic:all_true3(Conditions) of
        true
        	->	spawn_child(Network_Info,Workstack,Process_Delegation);
        false
        	->	#network_workstack{network_info=Network_Info,workstack=Workstack}
    end.

%%
%%	Creates a new process delegating Workstack to it.  The new process will become a child
%%	of this one, i.e. have its Pid added to the children_processes set in Network_Info.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Workstack: The stack of work to be delegated to the spawned process
%%		[#sat_state]
%%	Process_Delegation: Information required to spawn a new process process for this network
%%					 or to respond to a global lack of work
%%		#process_delegation
%%
spawn_child(Network_Info,Workstack,Process_Delegation) ->
    Work_Splitting_Fun = (Network_Info#network_info.network_config)#network_config.work_splitting_fun,
    {My_workstack,Child_Workstack} = Work_Splitting_Fun(Workstack),
    New_Child = spawn(process_tree_util,init_child_process,[create_child_network_info(Network_Info),Child_Workstack,Process_Delegation]),
    link(New_Child),
    New_Children_Processes = sets:add_element(New_Child,Network_Info#network_info.children_processes),
	Network_Info2 = erlang:setelement(#network_info.children_processes,Network_Info,New_Children_Processes),
    Network_Info3 = erlang:setelement(#network_info.process_count,Network_Info2,Network_Info2#network_info.process_count+1),
    Network_Info4 = erlang:setelement(#network_info.process_total,Network_Info3,Network_Info2#network_info.process_total+1),
    broadcast_message(Network_Info4,#network_change{notification=increase,notifier_pid=self()},cancel_message),
    #network_workstack{network_info=Network_Info4,workstack=My_workstack}.

%%
%%	Returns a new #network_info which is a child of this tree process.
%%	The new #network_info will have the current process as a parent_process, an empty
%%	children_processes and a process_count equal to the process_count in Network_Info.
%%	
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%
create_child_network_info(Network_Info) ->
    New_Network_Info = erlang:setelement(#network_info.parent_process,Network_Info,self()),
    erlang:setelement(#network_info.children_processes,New_Network_Info,sets:new()).

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
init_child_process(Network_Info,Workstack,Process_Delegation) ->
	New_Network_Info = await_parent_notification(Network_Info),	
    Module = Process_Delegation#process_delegation.module,
    Delegation_Fun = Process_Delegation#process_delegation.delegation_fun,
    Additional_Args = Process_Delegation#process_delegation.delegation_args,
    apply(Module,Delegation_Fun,[New_Network_Info,Workstack|Additional_Args]).

%%
%%	This function returns a #network_info which is the same as the one provided except with an 
%%	incremented process_count.  This function waits on the mailbox until it receives a message.
%%	This message is assumed to contain a #network_change from this process's parent.  The process change
%%	is processed and this function returns with the updated #network_info i.e. with a incremented
%%
%%	The purpose of this function is to ensure that a created process (or any of its children)will
%%	never retire and broadcast its retirement #network_change message before the #network_change message
%%	associated with its birth has been broadcast.  If this were to happen it is possible that 
%%	processes in the network might have a process count of 0 while some processes were still actually working
%%	because they had received a process's retirement notice before they received its birth notice.
%%
%%	Network_Info: Contains information locating this new process in the process tree
%%		#network_info
%%
await_parent_notification(Network_Info) ->
    receive
		Message when is_record(Message#network_message.message,network_change)
            ->	New_Network_Info = process_network_change(Network_Info,Message),
            	New_Network_Info
    end.
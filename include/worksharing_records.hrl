%% Author: Francis Stephens
%% Created: 16 Jun 2008
%% Description: TODO: Add description to process_records

%%
%% Records
%%

%%%
%%%	A #work_request contains the Pid of a process who is currently out of work
%%%
%%%		requester_pid: The Pid of the process seeking work.
%%%			Pid()
%%%
-record(work_request,{requester_pid}).

%%%
%%%	A #work_response contains a stack of Sat_States for processing.
%%%
%%%		workstack: stack of #sat_state for processing
%%%			[#sat_state]
%%%		responder_pid: The Pid of the process responding to this request, useful for debugging purposes.
%%%			Pid()
%%%
-record(work_response,{workstack,responder_pid}).

%%%
%%%	A #work_complete is a message which will cause all processes who receive it to cease working.
%%%
%%%		notifier_pid: The Pid of the process who originally sent this message, useful for debugging purposes
%%%			Pid()
-record(work_complete,{notifier_pid}).

%%%
%%%	A #network_change is used to inform process in the network that a process has either been spawned or 
%%%	has committed suicide/retired so they can maintain an approximate size of the tree.
%%%
%%%		notification: Indicates whether a process has been spawned (increase) or suicided/retired (decrease)
%%%			increase | decrease
%%%		notifier_pid: The Pid of the process who originally sent this message, useful for debugging purposes
%%%			Pid()
%%%		
-record(network_change,{notification,notifier_pid}).

%%%
%%%	A #network_and_workstack contains both the #network_info record describing a particular process
%%%	in a tree communication network as well as the work_stack containing the work currently available
%%%	to that process.
%%%
%%%		network_info: The #network_info describing this process
%%%			#network_info
%%%		workstack: A stack containing all of the work available to this process
%%%			[#sat_state]
%%%
-record(network_workstack,{network_info,workstack}).

%%%
%%%	A #process_delegation contains all of the information required to spawn a new process from a process tree.
%%%	A new process, after initialising itself, calls delegation_fun from module with (in this order) the arguments
%%%		1: #network_info
%%%		2: [#sat_state]
%%%		3: Any arguments provided in the list additional_args
%%%
%%%		module: The module from which to call delegation_fun
%%%			Atom()
%%%		delegation_fun: The function (of at least arity 2) which does the work delegated to this process.
%%%						The first two arguments to this function must be #network_info and [#sat_state]
%%%			Fun()
%%%		delegation_args: Arbitrary additional (beyond the first two) arguments for delegation_fun
%%%			[Term()]
%%%		no_work_fun: The function which handles the case where no work remains for
%%%					 any process in the network.
%%%			Fun()
%%%		no_work_args: Arbitrary additional arguments for no_work_fun
%%%			[Term()]
%%%		work_complete_fun: The function which handles the case where a positive result has been found.
%%%						   This function will be called from the function process_list_util:work_complete(Network_Info,Process_Delegation,Success_Arg) 
%%%						   The Success_Arg will be passed back into the function defined here as the first argument
%%%						   with additional arguments provided by work_complete_args.
%%%			fun()
%%%		work_complete_args: Arbitrary additional arguments for work_complete_fun
%%%			[Term()]
%%%
-record(process_delegation,{module,delegation_fun,delegation_args,no_work_fun,no_work_args,work_complete_fun,work_complete_args}).
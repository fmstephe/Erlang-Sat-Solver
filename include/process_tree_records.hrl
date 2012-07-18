%% Author: Francis Stephens
%% Created: 16 Jun 2008
%% Description: TODO: Add description to process_records

%%
%%	Includes
%%

%%
%%	Macros
%%

%%
%%	The name of the root node in a process tree
%%
-define(ROOT,root).

%%
%% Records
%%

%%%
%%%	A #network_message is the structure used as the primary message propogating
%%%	protocol for message passing in process_trees.
%%%
%%%	The structure of a #network_message relies on processes adherring to the common
%%%	communication protocol described below.
%%%
%%%	NB: All processes propogated through the network using this protocol are said to traverse
%%%	the tree in downwards first and left to right fashion.
%%%
%%%	A process receiving a #network_message must determine whether it ought to
%%%	propogate the message further.  If it wishes to it then pops its Pid from the top of
%%%	the Forwarding_Stack and pushes (in this order) the Pid of its parent processor
%%%	and the Pids of its children onto the Forwarding_Stack provided they are not in
%%%	Visited.  If the process does not put one of its children onto the Forwarding_Stack
%%%	because it was in Visited then that Pid is removed from Visited because it will
%%%	never be put onto the Forwarding_Stack again, so we may forget about it.  Finally
%%%	the top of Forwarding_Stack is peeked at and if the value is a Pid then the
%%%	#network_message is forwarded to that Pid.  Values pushed onto the Forwarding_Stack
%%%	are not limited to Pids but non-Pid values (by convention the atom 'cancel_message')
%%%	will result in the message being abandoned.
%%%
%%%	If the root process (the process with no parent) receives a #network_message it
%%%	follows the procedure described above except that before it pushes any Pids onto the
%%%	Forwarding_Stack it will first push Final_Destination.  The Final_Destination could be
%%%	the Pid of the originator of this message (who may be interested that the message has
%%%	been propogated through the entire tree) or may be a non-Pid value such as the atom
%%%	'cancel-message'.
%%%	
%%%	Processes which produce messages which may eventually be returned to the producer
%%%	from the root process must be able to deal with these messages.  It is important to note
%%%	that a message which is returned to the producer and propogated from there may
%%%	potentially be an infinitely looping message.
%%%	
%%%	#work_request is forwarded through the entire tree and no one has responded the
%%%	requesting process is informed.  This may cause the requesting process to commit
%%%	suicide, retire or just go to sleep for a while before requesting work again.
%%%
%%%	As an example a #work_request sent from a process with Pid 123abc, whose children
%%%	processes Pids are c111, c222, c333 and whose parent Pid is p111, initially will 
%%%	look like: {network_message,[123abc],[c111,c222,c333,p111],123abc,{work_request,123abc}}
%%%
%%%		visited: Set of processes (Pids) that have been visited (and are in danger of being visited again)
%%%			Set()
%%%		forwarding_stack: Stack of processes to which this #work_request will be forwarded
%%%			[Pid()]
%%%		Final_Destination: The destination of this message when it has been sent to every process in the tree
%%%			Term() | cancel_message | Pid
%%%		message: The message which is to be propogated through the network
%%%			Term()
%%%		time_sent: The time recorded when this message was sent.
%%%				   NB: This is recorded in the same format as provided by the function call erlang:time/0
%%%			{Integer(),Integer(),Integer()}
%%%
-record(network_message,{visited_processes,forwarding_stack,final_destination,message,time_sent}).

%%%
%%%	A #network_info contains the structural information required to descibe a process in
%%%	a tree communication network.  A process in a network has a single parent process and a number of child processes.
%%%	In any tree network there is a single process which has no parent.   This process is called the 
%%%	root and the value of its parent_process is the atom 'root'.  Nodes whose children_processes list
%%%	is empty are called leaf processes and processes who have both a parent and at least one child process
%%%	are called internal processes (by usual convention).
%%%
%%%	parent_process: The parent process of this process process
%%%		Pid() | root
%%%	children_processes: The set of children processes for this process process
%%%		Set()
%%%	process_count: maintains a local count of the number of processes currently in the tree
%%%				   NB: This count is not guaranteed accurate, it is based on received information
%%%		Integer()
%%%	process_total: Maintains a count of the total number of processes created for this process tree.
%%%				   While process_count maintains the current number of live processes, process total
%%%				   also includes the number of retired processes.
%%%	network_config: The configuration info for this network process
%%%		#network_config
%%%
-record(network_info,{parent_process,children_processes,process_count,process_total,network_config}).

%%%
%%%	A #network_config contains all of the information required to configure a process tree.  These
%%%	settings are passed into the process_tree functions
%%%
%%%	max_tree_size: The maximum number of nodes allowable in the process_tree
%%%		Integer()
%%%	process_spawn_threshold: The amount of work that must be available for a new process to be spawned
%%%		Integer()
%%%	work_sharing_threshold: The amount of work that must be available for work to be shared
%%%		Integer()
%%%	work_splitting_fun: A function which takes a single list and returns two lists which together make up
%%%						the original list.  This function determines how work is divided when it is shared.
%%%		fun([Term()]) -> {[Term()],[Term()]}
%%%	log_name_map: The name of the log file for this process list
%%%		String()
%%%
-record(network_config,{max_tree_size,process_spawn_threshold,work_sharing_threshold,work_splitting_fun,log_name}).
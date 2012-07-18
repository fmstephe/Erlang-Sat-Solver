%% Author: Francis Stephens
%% Created: 16 Jun 2008
%% Description: TODO: Add description to process_records

%%
%%	Includes
%%

%%
%% Records
%%

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
-record(distributed_network_info,{top_process,bottom_process,process_count,network_config}).

%%%
%%%	A #distributed_network_config contains all of the information required to configure a distributed process list.  These
%%%	settings are passed into the process_tree functions
%%%
%%%	nodes: A list of nodes which make up the actual network for this process list
%%%		Node()
%%%	list_network_configs: A list of #list_network_config where |nodes| = |list_network_configs| and each memmber of this list
%%%		is a configuration of its corresponding node
%%%		#list_network_config
%%%
-record(distributed_network_config,{nodes,list_network_configs}).
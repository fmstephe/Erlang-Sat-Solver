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
%%	Log names
%%

%%
%%	The message sent to a hibernating process which will wake it up
%%
-define(WAKE_UP,wake_up).

%%
%%	The atom signifying the top of a process list
%%
-define(TOP_OF_LIST,top_of_list).

%%
%%	The atom signifying the bottom of a process list
%%
-define(BOTTOM_OF_LIST,bottom_of_list).

%%
%%	The number of milliseconds that a process which has received
%%	its own work request (a poison work request) will wait initially
%%	before asking for work again.  processes which repeatedly request
%%	work and receive those requests back will double the backoff
%%	between consecutive failed requests.
%%
-define(INITIAL_BACKOFF,1000).

%%
%% Records
%%

%%%
%%%	A #list_network_message is the structure used as the primary message propogating
%%%	protocol for message passing in list based networks.  
%%%
%%%	The structure of a #network_message relies on processes adherring to the common
%%%	communication protocol described below.
%%%
%%%	The strict order in which a message makes its way through the network is not defined
%%%	here.  It is, however, required that every process (and node) in the list network 
%%%	be reachable by any message from any part of the network.
%%%
%%%	For instance a possible mechanism for propogating messages through the network could
%%%	be to follow the bottom_process recursively, i.e. from here go to the end of the list,
%%%	until the end of the list is reached and then jump back to the top_process and follow it
%%%	to the start of the list.  Alternatively we could equally jump back and forth following
%%%	first the bottom_process and then the top_process working our way simultaneously toward 
%%%	both the start and end of the list.  The second approach would have the advantage of tending
%%%	to find replies to work requests closer to the requester w.r.t. their positions in the list.
%%%
%%%	A process receiving a #list_network_message must determine whether it ought to propogate
%%%	the message further.  If it wishes to propogate the message it must first determine the 
%%%	direction from which the message came (this is done by comparing self() to both 
%%%	bottom_process and top_process).
%%%
%%%		A)	If the message came from above then the bottom_process element will be the Pid of this
%%%			process.  This Pid must be replaced with the Pid of the process below this one in the
%%%			list and the message forwarded on.
%%%		B)	If the message came from below then the top_process element will be the Pid of this
%%%			process.  This Pid must be replaced with the Pid of the process above this one in the
%%%			list and the message forwarded on.
%%%		C)	If the message has only just been created and has not yet been propogated both the 
%%%			top and bottom processes will not match the Pid of the current process.  In this case
%%%			the message can be forwarded up or down the list unchanged.
%%%
%%%	A process may forward a message, after completing either A, B or C above, by sending the message
%%%	to either the top_process or the bottom_process.  If the message is sent to the top_process 
%%%	the received_from element must be 'below'.  Conversely if the message is sent to the bottom_process 
%%%	the received_from element must be 'above'.
%%%
%%%	The two ends of a network list are labelled 'top_of_list' and 'bottom_of_list' respectively.
%%%	If the top_process is not a Pid but the atom 'top_of_list' we are unable to propogate a
%%%	to the top_process as per normal.  The same applies when the bottom_process is the atom
%%%	'bottom_of_list'.  If one of either top_process or bottom_process are disabled in this way
%%%	then the message is propogated only to the remaining destination.  When both are disabled
%%%	the message is sent to the process indicated by final_destination provided final_destination
%%%	is a Pid, otherwise some other course of action may be taken based on the value of
%%%	final_destination.  By convention when final_destination is the atom 'cancel_message' the
%%%	message is discarded.
%%%
%%%	Processes which produce messages which may eventually be returned to them must be able to 
%%%	deal with these messages.  It is important to note that a message which is returned to the
%%%	originating process and propogated from there may potentially be an infinitely looping message.
%%%	
%%%		top_process: The next visitable process toward the top of the list or 'top_of_list' 
%%%					 the top of the list has been reached.
%%%			Pid() | top_of_list
%%%		bottom_process: The next visitable process toward the bottom of the list or 
%%%						'bottom_of_list' if the bottom of the list has been reached.
%%%			Pid() | bottom_of_list
%%%		final_destination: The destination of this message when it has been sent to every process in the tree
%%%			Term() | cancel_message | Pid
%%%		message: The message to be propogated
%%%			Term()
%%%		time_sent: The time recorded when this message was sent.
%%%				   NB: This is recorded in the same format as provided by the function call erlang:time/0
%%%			{Integer(),Integer(),Integer()}
%%%
-record(list_network_message,{top_process,bottom_process,final_destination,message,time_sent}).

%%%
%%%	A #list_network_info contains the structural information required to descibe a process in
%%%	a list communication network.  A process in a network has a single parent process and a single child
%%%	process.  In any list network there is a single process which has no parent and a single process 
%%%	which has no child.  These processes are respectively called the top_of_list and bottom_of_list.
%%%
%%%	top_process: The process which is above this process in the process list
%%%		Pid() | top_of_list
%%%	bottom_process: The process which is below this process in the process list
%%%		Pid() | bottom_of_list
%%%	process_count: maintains a local count of the number of processes currently in the tree
%%%				   NB: This count is not guaranteed accurate, it is based on received information
%%%		Integer()
%%%	network_config: The configuration info for this network process
%%%		#network_config
%%%
-record(list_network_info,{top_process,bottom_process,process_count,network_config}).

%%%
%%%	A #list_network_config contains all of the information required to configure a process list.  These
%%%	settings are passed into the process_list functions
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
-record(list_network_config,{network_size,work_sharing_threshold,work_splitting_fun,log_name}).
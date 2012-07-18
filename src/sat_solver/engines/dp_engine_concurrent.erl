%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description:	Provides functions for traversing the search tree of a given Davis-Putnam SAT solver where
%%				rather than terminate search at the first found satisfying variable assignment we continue
%%				searching allowing us to explore the shape of the 'entire' search tree and discover the 
%%				locations of other satisfying variable assignments.
-module(dp_engine_concurrent).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("process_records.hrl").
-include("sat_macros.hrl").
-include("worksharing_records.hrl").
-include("dp_engine_concurrent_records.hrl").

%%
%%	Records
%%

%%
%% Exported Functions
%%
-export([is_satisfiable/2,
		 delegate_workstack/6,
		 handle_unsatisfiable/1,
		 handle_work_complete/2,
		 get_default_args/0]).

%%
%% API Functions
%%

%%
%%	Returns a Sat_Result indicating whether the #generated_formula provided is satisfiable or unsatisfiable.
%%	Takes a CNF formula and traverses the search tree defined by the sat algorithm provided aggregating the 
%%	search tree structure of the tree as it runs.
%%
%%	Traverse_sate_tree applies each of the provided functions to the formula provided.
%%	Each function must take a #sat_state as an argument and return a #sat_result.
%%	The functions are divided into three groups:
%%		1: A set of pre-functions
%%		2: A single function for retrieving an unassigned literal
%%		3: A set of post-functions
%%
%%	Pre-Functions phase:
%%	Each of the pre-functions is applied to sucessive #sat_states and if the formula is satisfied we return this fact to the 
%%	Caller_Pid including a satisfying assignment for the formula.  Unsatisfiable formulae are discarded until there is no work
%%	left.
%%
%%	Split phase:
%%	After the pre-functions we select a single variable and branch the search tree by assigning the variable true 
%%	and then false (or the other way around).  This creates extra work.
%%
%%	Post-Functions phase:
%%	Each of the post-functions is applied to sucessive #sat_states and if the formula is satisfied we return this fact to the 
%%	Caller_Pid including a satisfying assignment for the formula.  Unsatisfiable formulae are discarded until there is no work
%%	left.
%%
%%	In addition to the sequential processes described above this module employs a process tree to split the work among many
%%	processes.  In order to manage the process tree well this function requires that the #dp_funs provided include engine_args
%%	of type #engine_concurrent_args.  These args contain both a #network_config record and an integer determining how often (in
%%	terms of main loop cycles) the process tree is maintained.
%%
%%	Calls to this function are entirely insulated from the many-process nature of this function.  A #sat_result is returned in the
%%	usual manner like each of the other solver engines.
%%
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Formula: The CNF formula to be solved
%%			#formula
%%
is_satisfiable(Dp_Funs,Generated_Formula) ->
    Caller_Pid = self(),
	Listener_Fun = 	fun() ->
                      	Listener_Pid = self(),
                  		Init_Fun = 	fun() -> 
               							Init_Sat_State = Dp_Funs#dp_funs.init_sat_state,
										Formula_Def = Generated_Formula#generated_formula.formula_def,
    									Initial_Sat_State = Init_Sat_State(Formula_Def),
                                        Engine_Args = get_specific_args(Dp_Funs#dp_funs.engine_args),
                                        Comm_Frequency = Engine_Args#engine_concurrent_args.comm_freq,
                                        Network_Config = Engine_Args#engine_concurrent_args.network_config,
                                        Process_Tree_Module = Engine_Args#engine_concurrent_args.process_tree_mod,
                                        Process_Delegation = create_process_delegation(Dp_Funs,Comm_Frequency,Process_Tree_Module,Listener_Pid),
                                        Network_Info = Process_Tree_Module:create_worksharing_network(Network_Config,Process_Delegation),
										main_loop(Dp_Funs,[Initial_Sat_State],Network_Info,Comm_Frequency,Process_Tree_Module,Listener_Pid,0)
									end,
    					spawn(Init_Fun),
						receive
							Message when is_record(Message,sat_result)
								->	Caller_Pid ! Message,
									exit(terminating_network)
						end
					end,
	%% Here we spawn a listener process which will in turn spawn a solver
	%% The listener waits for the first message back from the solver, communicates
	%% this message back to this calling process and then terminates itself allowing
	%% any remaining solver processes to terminate gracefully and post their final messages
	%% into the ether (i.e. nowhere).
	%% Without the disposable listening process the solver processes will send some
	%% number of messages (which is not known by the caller) filling up its mailbox
	%% indefinitely with spurious sat_result messages.
	spawn(Listener_Fun),
	receive
		Message when is_record(Message,sat_result)
			->	Message
	end.

%%
%%	Extracts the additional arguments required for configuring this concurrent solver engine.
%%	If the arguments are not found default ones are supplied.
%%
%%	Engine_Args: The arguments supplied, which may or may not be what we seek
%%		Term() | #engine_concurrent_args
%%
get_specific_args(Engine_Args) ->
    case erlang:is_record(Engine_Args,engine_concurrent_args) of
		true
			->	Engine_Args;
        false
  			->	get_default_args()
    end.

%%
%%	Returns the default #engine_concurrent_args.  These will be used if a #engine_concurrent_args are not
%%	provided in #dp_funs.engine_args in a call to is_satisfiable/2.
%%
get_default_args() ->
    Network_Config = process_tree_util:get_default_config(),
    #engine_concurrent_args{network_config=Network_Config,comm_freq=50,process_tree_mod=process_tree}.

%%
%%	This is function which is called by the process tree when available work is to be delegated to a
%%	process.  This function has no real return type as it will never return.  Instead the calling process
%%	will eventually terminate and may or may not send a message to Caller_Pid.  It is guaranteed that at least
%%	one process in the process tree will send such a message eventually.
%%
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Workstack: The current stack of work available to this process
%%		[#sat_state]
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%	Comm_Frequency: The frequency (in terms of main loop cycles) at which the process tree is maintained
%%		Integer()
%%	Process_Tree_Module: The specific process tree implementation to use
%%		Atom()
%%	Caller_Pid: The Pid() of the calling process to whom messages can be sent if an answer is found
%%		Pid()
%%
delegate_workstack(Network_Info,Workstack,Dp_Funs,Comm_Frequency,Process_Tree_Module,Caller_Pid) ->
	main_loop(Dp_Funs,Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,0).

%%
%%	This the function which is called when it is determined that there is no more work available across
%%	the entire process tree.  This means that the formula is not satisfiable and a message communicating
%%	will be sent to the Pid() of the process who initiated this sat-search.
%%
%%	Caller_Pid: The Pid() of the process who initiated this sat-search
%%		Pid()
%%
handle_unsatisfiable(Caller_Pid) ->
	Caller_Pid ! #sat_result{satisfiable=false,sat_state=null}.

%%
%%	This function is called when a satisfying variable assignment has been found.
%%	The satisfying assignment is sent back to the process which is waiting for a
%%	response.
%%
%%	Sat_Result: The satisfying assignment for this formula
%%		#sat_result
%%	Caller_Pid: The Pid() of the process who initiated this sat-search
%%		Pid()
%%
handle_work_complete(Sat_Result,Caller_Pid) ->
	Caller_Pid ! Sat_Result.

%%
%% Local Functions
%%

%%
%%	Returns a #sat_result indicating if the processing done rendered the #sat_state provided satisfiable,
%%	unsatisfiable or if it remains as indetermined.  The processing functions provided are applied in turn
%%	where each function takes a #sat_state and and returns a #sat_result.  The #sat_state contained in this
%%	#sat_result is feed into the next processing function like a standard fold function.
%%
%%	If any of the processing functions return a #sat_state which is satisfiable or unsatisfiable we return
%%	immediately, otherwise the final #sat_state produced is returned inside a #sat_result with satisfiability
%%	set to indetermined.
%%
%%	Process_Funs: A list of processing functions which take a single #sat_state as argument and return a #sat_result
%%		[fun()/1]
%%	Sat_State: A #sat_state which will be processed by each of the functions in Process_Funs
%%		#sat_state
%%
process_state([],Sat_State) ->
	#sat_result{satisfiable=indetermined,sat_state=Sat_State};

process_state([Process_Fun|Process_Funs],Sat_State) ->
	case Process_Fun(Sat_State) of
        {sat_result,indetermined,New_State}
			->	process_state(Process_Funs,New_State);
		Sat_Result %% Here the formula is either satisfied or unsatisfiable
			->	Sat_Result
	end.

%%
%%	(Clearly) This is the main loop of this sat solver engine.
%%
%%	This function has no real return type as it will never return.  Instead the calling process will eventually
%%	terminate and may or may not send a message to Caller_Pid.  It is guaranteed that at least one process in 
%%	the process tree will send such a message eventually.
%%
%%	If the workstack is empty then we will appeal to the network to ask for more work to do.
%%	
%%	If we have managed the number of main loop iterations indicated by Comm_Frequency then we will
%%	perform our regular network maintenance.  This should be done regularly so as all of the messages
%%	we have been sent don't build up and leave us as a system bottle-neck.  This should not be done so
%%	regularly that we don't do much work for all the network maintenance that we are doing :)
%%	
%%	Otherwise we actually do some SAT solving. (Moderately nasty group of nested case statements)
%%		1: First each of the pre_funs are run on the current #sat_state.
%%				If this does not answer satisfiability we,
%%		2: Branch on assigning a chosen variable to either true or false, pushing the results onto the Workstack
%%		3: We run each of the post_funs on the #sat_state popped off the top of the Workstack
%%				If this does not answer satisfiability we,
%%		4: Call the main_loop again as a last resort.
%%		
%%	Dp_Funs: Collection of functions defining a Davis Putnam style sat solver.
%%		#dp_funs
%%	Workstack: Stack of work to be done (read list of #stat_state)
%%		[#sat_state]
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Comm_Frequency: Defines the frequency (in terms of main loop cycles) at which the process tree is maintained
%%		Integer()
%%	Process_Tree_Module: The specific process tree implementation to use
%%		Atom()
%%	Caller_Pid: The Pid() of the calling process to whom messages can be sent if an answer is found
%%		Pid()
%%	Comm_Count: Counter for counting the number of iterations done in the main_loop since last network maintenance
%%		Integer()
%%

%%	Empty workstack - appeal to network for more work
%%	NB: Some process tree automatically retire processes without work
main_loop(Dp_Funs,[],Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,_Comm_Count) ->
	Process_Delegation = create_process_delegation(Dp_Funs,Comm_Frequency,Process_Tree_Module,Caller_Pid),
    Network_Workstack = Process_Tree_Module:manage_network(Network_Info,[],Process_Delegation),
	New_Workstack = Network_Workstack#network_workstack.workstack,
	New_Network_Info = Network_Workstack#network_workstack.network_info,
	main_loop(Dp_Funs,New_Workstack,New_Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,0);

%%	We have cycled the main loop Comm_Frequency times since the last manage_network call.
%%	Time to manage the network again.
main_loop(Dp_Funs,Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Frequency) ->
	Process_Delegation = create_process_delegation(Dp_Funs,Comm_Frequency,Process_Tree_Module,Caller_Pid),
    Network_Workstack = Process_Tree_Module:manage_network(Network_Info,Workstack,Process_Delegation),
	New_Workstack = Network_Workstack#network_workstack.workstack,
	New_Network_Info = Network_Workstack#network_workstack.network_info,
	main_loop(Dp_Funs,New_Workstack,New_Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,0);
    
%%	Real sat-solver loop do some real work here.
main_loop(Dp_Funs,[Sat_State|Workstack],Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Counter) ->
	case process_state(Dp_Funs#dp_funs.pre_funs,Sat_State) of
		{sat_result,indetermined,Sat_State_Pre}
			->	[Left_Branch_State|New_Workstack] = (Dp_Funs#dp_funs.branch_fun)([Sat_State_Pre|Workstack]),
				case process_state(Dp_Funs#dp_funs.post_funs,Left_Branch_State) of
					{sat_result,indetermined,Sat_State_Post}
						->	main_loop(Dp_Funs,[Sat_State_Post|New_Workstack],Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Counter+1);
					Sat_Result
						->	check_sat_result(Sat_Result,Dp_Funs,New_Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Counter+1)
				end; %% end post-processing
		Sat_Result
			->	check_sat_result(Sat_Result,Dp_Funs,Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Counter+1)
	end. %% end pre-processing

%%
%%	This function has no real return type as it will never return.  Instead the calling process
%%	will eventually terminate and may or may not send a message to Caller_Pid.  It is guaranteed that at least
%%	one process in the process tree will send such a message eventually.
%%
%%	If the Sat_Result provided is satisfiable (#sat_result.satisfiable = true) then we notify the Caller_Pid of this and 
%%	conclude all searching by calling work_complete on the process tree.  Otherwise the main loop is called and search 
%%	continues in the usual manner.
%%
%%	Sat_Result: The #sat_result to check for satisfiability
%%		#sat_result
%%	Dp_Funs: Collection of functions defining a Davis Putnam style sat solver.
%%		#dp_funs
%%	Workstack: Stack of work to be done (read list of #stat_state)
%%		[#sat_state]
%%	Network_Info: Contains information locating this process in the process tree
%%		#network_info
%%	Comm_Frequency: Defines the frequency (in terms of main loop cycles) at which the process tree is maintained
%%		Integer()
%%	Process_Tree_Module: The specific process tree implementation to use
%%		Atom()
%%	Caller_Pid: The Pid() of the calling process to whom messages can be sent if an answer is found
%%		Pid()
%%	Comm_Count: Counter for counting the number of iterations done in the main_loop since last network maintenance
%%		Integer()
%%
check_sat_result(Sat_Result,Dp_Funs,_Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,_Comm_Counter) when Sat_Result#sat_result.satisfiable =:= true ->
	Process_Delegation = create_process_delegation(Dp_Funs,Comm_Frequency,Process_Tree_Module,Caller_Pid),
	Process_Tree_Module:work_complete(Network_Info,Process_Delegation,Sat_Result);

check_sat_result(_Sat_Result,Dp_Funs,Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Counter) ->
    main_loop(Dp_Funs,Workstack,Network_Info,Comm_Frequency,Process_Tree_Module,Caller_Pid,Comm_Counter).

%%
%%	Returns a #process_delegation which allows the process tree to call back into this module to either delegate sat solving
%%	work to a new process or handling the case where work has run out across the entire network.
%%
%%	Dp_Funs: Collection of functions defining a Davis Putnam style sat solver.
%%		#dp_funs
%%	Comm_Frequency: Defines the frequency (in terms of main loop cycles) at which the process tree is maintained
%%		Integer()
%%	Process_Tree_Module: The specific process tree implementation to use
%%		Atom()
%%	Caller_Pid: The Pid() of the calling process to whom messages can be sent if an answer is found
%%		Pid()
%%
create_process_delegation(Dp_Funs,Comm_Frequency,Process_Tree_Module,Caller_Pid) ->
	#process_delegation{module=dp_engine_concurrent,
                            delegation_fun=delegate_workstack,
                            delegation_args=[Dp_Funs,Comm_Frequency,Process_Tree_Module,Caller_Pid],
                            no_work_fun=handle_unsatisfiable,
                            no_work_args=[Caller_Pid],
							work_complete_fun=handle_work_complete,
					   		work_complete_args=[Caller_Pid]}.
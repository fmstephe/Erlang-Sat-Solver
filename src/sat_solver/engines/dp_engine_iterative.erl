%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description:	Provides functions for traversing the search tree of a given Davis-Putnam SAT solver where
%%				rather than terminate search at the first found satisfying variable assignment we continue
%%				searching allowing us to explore the shape of the 'entire' search tree and discover the 
%%				locations of other satisfying variable assignments.
-module(dp_engine_iterative).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("process_records.hrl").
-include("sat_macros.hrl").

%%
%%	Records
%%

%%
%% Macros
%%

-define(COMM_FREQUENCY,20).

%%
%% Exported Functions
%%
-export([is_satisfiable/2]).

%%
%% API Functions
%%

%%
%%	Function which takes a CNF formula and traverses the search tree
%%	defined by the sat algorithm provided aggregating the search tree structure
%%	of the tree as it runs.
%%
%%	This is the point of entry for processes provided with a particular search tree branch for traversal.
%%	That is why this function takes in a number of arguments which assume prior traversal has occurred.
%%
%%	traverse_sate_tree applies each of the provided functions to the formula provided.
%%	Each function must take a single formula as an argument and return a #sat_result.
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
%%	and then false (or the other way around).
%%
%%	Post-Functions phase:
%%	Each of the post-functions is applied to sucessive #sat_states and if the formula is satisfied we return this fact to the 
%%	Caller_Pid including a satisfying assignment for the formula.  Unsatisfiable formulae are discarded until there is no work
%%	left.
%%
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Formula: The CNF formula to be solved
%%			#formula
%%
is_satisfiable(Dp_Funs,Generated_Formula) ->
    Init_Sat_State = Dp_Funs#dp_funs.init_sat_state,
	Formula_Def = Generated_Formula#generated_formula.formula_def,
    Initial_Sat_State = Init_Sat_State(Formula_Def),
	main_loop(Dp_Funs,[Initial_Sat_State]).

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
%%	If the workstack is empty then we will appeal to the network to ask for more work to do.
%%	
%%	If we have managed the number of main loop iterations indicated by ?COMM_FREQUENCY then we will
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
%%
main_loop(_Dp_Funs,[]) ->
    #sat_result{satisfiable=false};
    
main_loop(Dp_Funs,[Sat_State|Workstack]) ->
	case process_state(Dp_Funs#dp_funs.pre_funs,Sat_State) of
		{sat_result,indetermined,Sat_State_Pre}
			->	[Left_Branch_State|New_Workstack] = (Dp_Funs#dp_funs.branch_fun)([Sat_State_Pre|Workstack]),
				case process_state(Dp_Funs#dp_funs.post_funs,Left_Branch_State) of
					{sat_result,indetermined,Sat_State_Post}
						->	main_loop(Dp_Funs,[Sat_State_Post|New_Workstack]);
                    {sat_result,false,_Sat_State_Post}
						->	main_loop(Dp_Funs,[New_Workstack]);
					Sat_Result
						->	Sat_Result
				end; %% end post-processing
        {sat_result,false,_Sat_State_Pre}
            ->	main_loop(Dp_Funs,Workstack);
		Sat_Result
			->	Sat_Result
	end. %% end pre-processing
%% Author: Francis Stephens
%% Created: 8 Jan 2008
%% Description:	Provides functions for traversing the search tree of a given Davis-Putnam SAT solver where
%%				rather than terminate search at the first found satisfying variable assignment we continue
%%				searching allowing us to explore the shape of the 'entire' search tree and discover the 
%%				locations of other satisfying variable assignments.
-module(dp_engine_tree_agg).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("process_records.hrl").
-include("sat_macros.hrl").

%%
%% Exported Functions
%%
-export([is_satisfiable/2]).

%%
%%	This record encapsulates counters recording the number of nodes
%%	that have been traversed during search as well as the number of
%%	soutions found.
%%	The node_count is used to ensure that a unique name can be generated
%%	for every node that must be stored in the database.
%%	The solution count is used primarily to allow for solution limited
%%	searches where the solver will terminate upon finding a set number
%%	of solutions.
%%
-record(search_counters,{node_count,solution_count}).

%%
%%	This macro is used to define the limit on the number of solutions
%%	The value for this macro may be either a positive Integer() or the
%%	atom infinity.
%%
%%	NB: Using this value here instead of on the tree_agg server means that
%%		it is not possible to parallelise this sat_engine and limit the 
%%		number of solutions searched for.
%%	
%%		Integer() | infinity
%%
-define(SOLUTION_LIMIT,1000).

%%
%% API Functions
%%

is_satisfiable(Dp_Funs,Generated_Formula) ->
    Branch_Name = "branch",
    {ok,Aggregator_Pid} = gen_server:start(tree_agg_callback,[],[]),
    Coop_Info = #coop_info{branch_name=Branch_Name,delegator_id=null,aggregator_id=Aggregator_Pid},
	Init_Sat_State = Dp_Funs#dp_funs.init_sat_state,
    Tree_Metadata = #tree_metadata{	strategy_name=Dp_Funs#dp_funs.strategy_name,
                                    generated_formula=Generated_Formula},
    Formula_Def = Generated_Formula#generated_formula.formula_def,
	Initial_State = Init_Sat_State(Formula_Def),
    traverse_sat_tree_init(Dp_Funs,Initial_State,Coop_Info),
	Tree_Id = gen_server:call(Aggregator_Pid,last_tree_id),
	Attach_Tree_Metadata = #attach_tree_metadata{tree_id=Tree_Id,tree_metadata=Tree_Metadata},
	gen_server:call(Aggregator_Pid,Attach_Tree_Metadata),
	gen_server:call(Aggregator_Pid,satisfiable).

%%
%%	Function which takes a CNF formula and traverses the search tree
%%	defined by the sat algorithm provided aggregating the search tree structure
%%	of the tree as it runs.
%%
%%	This is the initial entry point for SAT search tree traversal.  When calling this function the formula
%%	to be solved and its search tree unelaborated.  This function is responsible for initialising a new search
%%	tree in the database and initiating SAT search tree traveral.
%%
%%	Where new processes are provided with parts of the search tree to traverse, i.e. during branch delegation.  The
%%	new branch should enter this module via traverse_sat_tree/7 found below.
%%
%%	traverse_sate_tree applies each of the provided functions to the formula provided.
%%	Each function must take a single formula as an argument and return a #sat_result.
%%	The functions are divided into three groups:
%%		1: A set of pre-functions
%%		2: A single function for retrieving an unassigned literal
%%		3: A set of post-functions
%%
%%	Pre-Functions phase:
%%	Each of the pre-functions is applied to sucessive #sat_states and if the formula is satisfied or demonstrated to be 
%%	unsatisfiable we terminate this search node and continue searching on the most recent unsearched variable 
%%	branch or finish searching altogether.
%%
%%	Split phase:
%%	After the pre-functions we select a single variable and branch the search tree by assigning the variable true 
%%	and then false (or the other way around).
%%
%%	Post-Functions phase:
%%	Finally each of the post-functions is applied to successive #sat_states and if the formula is satisfied or demonstrated 
%%	to be unsatisfiable we terminate this search node and continue searching on the most recent unsearched variable 
%%	branch or finish searching altogether.
%%
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Sat_State: The initial state containing both the initial formula and arbitrary state information
%%			#sat_state
%%		Coop_Info: Information required for tree aggregation and search tree delegation.
%%			#coop_info
%%
traverse_sat_tree_init(Dp_Funs,Sat_State,Coop_Info) ->
	pre_funs_init(Dp_Funs#dp_funs.pre_funs,Dp_Funs,Sat_State,Coop_Info).

%%
%%	The pre-functions are applied in turn and if one makes the formula satisfiable we terminate this search node
%%	and continue searching on the most recent unsearched variable branch or finish searching altogether.
%%	If there are no more pre_fun functions then we enter the split phase.
%%
%%		Pre_Funs: All of the functions that remain to be applied for this phase
%%			[fun/1] where each function takes a single #sat_state as argument and returns a #sat_results
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Sat_State: The state contains both the current formula and arbitrary state information
%%			#sat_state
%%		Coop_Info: Information required for tree aggregation and search tree delegation.
%%			#coop_info
%%
pre_funs_init([],Dp_Funs,Sat_State,Coop_Info) ->
    Tree_Root = aggregate_node_init(Coop_Info,indetermined),
    Root_Name = Tree_Root#tree_root.root_name,
    Tree_Id = Tree_Root#tree_root.tree_id,
	split(Dp_Funs,Sat_State,Coop_Info,1,0,Root_Name,Tree_Id);

pre_funs_init([Pre_Fun|Pre_Funs],Dp_Funs,Sat_State,Coop_Info) ->
	case Pre_Fun(Sat_State) of
        {sat_result,indetermined,New_State}
			->	pre_funs_init(Pre_Funs,Dp_Funs,New_State,Coop_Info);
		Sat_Result %% Here the formula is either satisfied or unsatisfiable
			->	aggregate_node_init(Coop_Info,Sat_Result#sat_result.satisfiable),
            	%% The search tree terminates here in a leaf node
            	generate_search_counter(1,1,Sat_Result)
	end.
                                                                                   
%%
%% Local Functions
%%

%%
%%	The pre-functions are applied in turn and if one makes the formula satisfiable we terminate this search node
%%	and continue searching on the most recent unsearched variable branch or finish searching altogether.
%%	If there are no more pre_fun functions then we enter the split phase.
%%
%%		Pre_Funs: All of the functions that remain to be applied for this phase
%%			[fun/1] where each function takes a single #sat_state as argument and returns a #sat_results
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Sat_State: The state contains both the current formula and arbitrary state information
%%			#sat_state
%%		Coop_Info: Information required for tree aggregation and search tree delegation.
%%			#coop_info
%%		Node_Count: A Unique number associated with each node in this search tree.
%%					 This number need only be unique w.r.t. all nodes created for Tree_Id.
%%					 Node numbers for different Tree_Ids will usually overlap.
%%		Parent_Name: The database name of the parent node of the currently processing search tree node.
%%			String()
%%		Tree_Id: The unique database id for the tree currently being processed
%%			integer
%%		Left_Or_Right: Indicates whether the node currently being generated in the search tree is the 
%%					   left or right handed child of its parent.
%%		Variable_Assignment: The last variable assignment, made during the split phase, to the current formula in Sat_State
%%			#variable_assignment
%%
pre_funs([],Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment) ->
	Node_Name = make_node_name(Coop_Info,Node_Count),	%% In call to split this will become the new Parent_Name
    aggregate_node(Coop_Info,Node_Name,Parent_Name,Tree_Id,Left_Or_Right,indetermined,Variable_Assignment),
	split(Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Node_Name,Tree_Id);

pre_funs([Pre_Fun|Pre_Funs],Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment) ->
	case Pre_Fun(Sat_State) of
        {sat_result,indetermined,New_State}
			->	pre_funs(Pre_Funs,Dp_Funs,New_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment);
		Sat_Result %% Here the formula is either satisfied or unsatisfiable
			->	Node_Name = make_node_name(Coop_Info,Node_Count),
				aggregate_node(Coop_Info,Node_Name,Parent_Name,Tree_Id,Left_Or_Right,Sat_Result#sat_result.satisfiable,Variable_Assignment),
            	generate_search_counter(Node_Count,Solution_Count,Sat_Result) %% The search tree terminates here in a leaf node
	end.

%%
%%	Split branches two ways by setting a chosen literal to true then false,
%%	We then move into the post_funs phase for each literal assignment (true or false)
%%
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Sat_State: The state contains both the current formula and arbitrary state information
%%			#sat_state
%%		Coop_Info: Information required for tree aggregation and search tree delegation.
%%			#coop_info
%%		Node_Count: A Unique number associated with each node in this search tree.
%%					 This number need only be unique w.r.t. all nodes created for Tree_Id.
%%					 Node numbers for different Tree_Ids will usually overlap.
%%		Parent_Name: The database name of the parent node for each of the search tree nodes about to be created.
%%			String()
%%		Tree_Id: The unique database id for the tree currently being processed
%%			integer()
%%
split(Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id) ->
    Choose_Variable = Dp_Funs#dp_funs.choose_variable,
	Variable_Assignment = {variable_assignment,L_Name,L_Value} = Choose_Variable(Sat_State),
    Assign_Variable = Dp_Funs#dp_funs.assign_variable,
    Left_Formula = Assign_Variable(Variable_Assignment,Sat_State#sat_state.formula),
	Left_State = #sat_state{formula=Left_Formula,state_metadata=Sat_State#sat_state.state_metadata},
	Post_Funs = Dp_Funs#dp_funs.post_funs,
	Search_Counters = {search_counters,Left_Node_Count,Left_Solution_Count} = post_funs(Post_Funs,Dp_Funs,Left_State,Coop_Info,Node_Count+1,Solution_Count,Parent_Name,Tree_Id,left,Variable_Assignment),
	case stats_util:max(Left_Solution_Count,?SOLUTION_LIMIT) =/= Left_Solution_Count of %% Here we make sure that we have not gather the limit number of solutions
		true
			->	Right_Formula = Assign_Variable({variable_assignment,L_Name,not L_Value},Sat_State#sat_state.formula),
				Right_State = #sat_state{formula=Right_Formula,state_metadata=Sat_State#sat_state.state_metadata},
				post_funs(Post_Funs,Dp_Funs,Right_State,Coop_Info,Left_Node_Count+1,Left_Solution_Count,Parent_Name,Tree_Id,right,{variable_assignment,L_Name,not L_Value});
		false
			->	Search_Counters
	end.

%%
%%	Finally we apply each of the post-functions and if one makes the formula satisfiable we terminate this search node
%%	and continue searching on the most recent unsearched variable branch or finish searching altogether.
%%	If there are no more pre_fun functions then we enter the pre_funs phase.
%%
%%		Post_Funs: All of the functions that remain to be applied for this phase
%%			[fun/1] where each function takes a single #sat_state as argument and returns a #sat_results
%%		Dp_Funs: Collection of functions which define a SAT algorithm.
%%			#dp_funs
%%		Sat_State: The state contains both the current formula and arbitrary state information
%%			#sat_state
%%		Coop_Info: Information required for tree aggregation and search tree delegation.
%%			#coop_info
%%		Node_Count: A Unique number associated with each node in this search tree.
%%					 This number need only be unique w.r.t. all nodes created for Tree_Id.
%%					 Node numbers for different Tree_Ids will usually overlap.
%%			integer()
%%		Parent_Name: The database name of the parent node of the currently processing search tree node.
%%			String()
%%		Tree_Id: The unique database id for the tree currently being processed
%%			integer()
%%		Left_Or_Right: Indicates whether the node currently being generated in the search tree is the 
%%					   left or right handed child of its parent.
%%		Variable_Assignment: The last variable assignment, made during the split phase, to the current formula in Sat_State
%%			#variable_assignment
%%
post_funs([],Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment) ->
	pre_funs(Dp_Funs#dp_funs.pre_funs,Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment);

post_funs([Post_Fun|Post_Funs],Dp_Funs,Sat_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment) ->
	case Post_Fun(Sat_State) of
        {sat_result,indetermined,New_State}
			->	post_funs(Post_Funs,Dp_Funs,New_State,Coop_Info,Node_Count,Solution_Count,Parent_Name,Tree_Id,Left_Or_Right,Variable_Assignment);
		Sat_Result %% Here the formula is either satisfied or unsatisfiable
			->	Node_Name = make_node_name(Coop_Info,Node_Count),
				aggregate_node(Coop_Info,Node_Name,Parent_Name,Tree_Id,Left_Or_Right,Sat_Result#sat_result.satisfiable,Variable_Assignment),
				generate_search_counter(Node_Count,Solution_Count,Sat_Result)
	end.

%%
%%	Creates a new tree in the database.
%%	Returns a #tree_root containing the details of the newly created tree root.
%%
%%		Coop_Info: Contains the server information required to create a new tree root
%%			#coop_info
%%		Satisfiable: Indicates whether this formula is satisfied (true) unsatisfiable (false) or indetermined (indetermined)
%%			true | false | indetermined
%%
aggregate_node_init(Coop_Info,Satisfiable) ->
	%% Note that all tree roots have the name "root"
	gen_server:call(Coop_Info#coop_info.aggregator_id,#create_tree{root_name="Root",satisfiable=Satisfiable}). 

%%
%%	Produces a name for the current node and sends this with the name
%%	of the parent node and this nodes satisfiability to the search tree
%%	aggregator.
%%
%%		Satisfied: Indicates whether this formula is satisfied (true) unsatisfiable (false) or indetermined (indetermined)
%%			true | false | indetermined
%%
aggregate_node(Coop_Info,Node_Name,Parent_Name,Tree_Id,Left_Or_Right,Satisfiable,Variable_Assignment) ->
	Node_Id = #node_id{node_name=Node_Name,tree_id=Tree_Id},
	Tree_Node = #tree_node{node_id=Node_Id,parent_name=Parent_Name,left_or_right=Left_Or_Right,satisfiable=Satisfiable,variable_assignment=Variable_Assignment},
	gen_server:cast(Coop_Info#coop_info.aggregator_id,Tree_Node).

%%
%%	Combines the unique name given to this sat searcher with the node number (unique w.r.t. this searcher) to create
%%	a node name unique w.r.t. any other node searched in this sat instance.
%%
make_node_name(Coop_Info,Node_Count) ->
	Coop_Info#coop_info.branch_name ++ util:int_to_string(Node_Count).

%%
%%	Generates a #search_counters record which may increment the Solution_Count value
%%	provided if the Sat_Result provided =:= satisfied.
%%	
%%		Node_Count: The current number of nodes recorded in the search tree
%%			Integer()
%%		Solution_Count: The current number of solutions recorded in the search tree
%%			Integer()
%%		Sat_Result: A #sat_result indicating whether or not the formula was satisfied
%%			#sat_result
%%
generate_search_counter(Node_Count,Solution_Count,Sat_Result) when Sat_Result#sat_result.satisfiable =:= true ->
	#search_counters{node_count=Node_Count,solution_count=Solution_Count+1};

generate_search_counter(Node_Count,Solution_Count,_Sat_Result) ->
	#search_counters{node_count=Node_Count,solution_count=Solution_Count}.
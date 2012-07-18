%% Author: Francis Stephens
%% Created: 19 Sep 2008
%% Description: TODO: Add description to solution_distance
-module(root_to_solution_metrics).

%%
%% Include files
%%
-include("process_records.hrl").

%%
%%	Macros
%%

%%
%%	The unique name of this tree metric
%%
-define(METRIC_TYPE,"Root To Solution Metrics").

%%
%%	Records
%%

%%
%%	An #root_to_solution_metrics record contains two lists of
%%	accumulated measurements of the tree being measured.
%%		1: path-lengths: The path-length for a given solution in a search tree
%%			is the distance from the tree root to the solution using a left to
%%			to right depth-first traversal of the tree.  This determines the distance
%%			travelled from the root to that solution by the sat solver used to develop
%%			the search tree being measured.
%%		2: solution-heights: The solution-height for a given solution in a search
%%			tree is the shortest distance from the tree root to that solution i.e.
%%			straight down.
%%
%%	path_lengths: List of path-length metrics for each solution in the search tree measured.
%%					NB: path_lengths and solution_heights have the property that where a solution
%%						appears in one list it also appears in the other list in the same list 
%%						position.  In addition the order in which the metrics appear in both lists
%%						is the same order in which they would be found in the search tree by left-right
%%						depth first traversal.  Therefore the first metric indicates the value of the 
%%						solution which would have been picked up by a single-threaded solution.
%%		[Integer()]
%%	solution_heights: List of solution-height metrics for each solution in the search tree measured.
%%		[Integer()]
%%
-record(root_to_solution_metrics,{path_lengths,solution_heights}).

%%
%% Exported Functions
%%
-export([compute_metric/1]).

%%
%% API Functions
%%

%%
%% TODO: Add description of compute_metric/function_arity
%%
compute_metric(Tree_Id) -> 
    Tree_Root = tree_persist:get_tree_root(Tree_Id),
	Raw_metrics = traverse_tree(Tree_Root),
	Raw_Path_Lengths = Raw_metrics#root_to_solution_metrics.path_lengths,
	Raw_Solution_Heights = Raw_metrics#root_to_solution_metrics.solution_heights,
	Root_To_Solution_Metrics = #root_to_solution_metrics{path_lengths=lists:reverse(Raw_Path_Lengths),
												solution_heights=lists:reverse(Raw_Solution_Heights)},
	#tree_metric{metric_type=?METRIC_TYPE,tree_id=Tree_Id,print_metric=fun print_metric/2,metric_data=Root_To_Solution_Metrics}.

%%
%% Local Functions
%%
traverse_tree(Tree_Root) ->
	Root_To_Solution_Metrics = #root_to_solution_metrics{path_lengths=[],solution_heights=[]},
	Sibling_Nodes = tree_persist:get_root_children(Tree_Root),
	Left_Sibling = Sibling_Nodes#sibling_nodes.left_sibling,
	Right_Sibling = Sibling_Nodes#sibling_nodes.right_sibling,
	traverse_tree([Left_Sibling,Right_Sibling],[1,1],Root_To_Solution_Metrics,1).

traverse_tree([],[],Root_To_Solution_Metrics,_Path_Length) ->
	Root_To_Solution_Metrics;

traverse_tree([undefined|Tree_Nodes],[Height|Heights],Root_To_Solution_Metrics,Path_Length) ->
    traverse_tree(Tree_Nodes,Heights,Root_To_Solution_Metrics,Path_Length);
    
traverse_tree([Tree_Node|Tree_Nodes],[Height|Heights],Root_To_Solution_Metrics,Path_Length) when Tree_Node#tree_node.satisfiable =:= true ->
	Path_Lengths = Root_To_Solution_Metrics#root_to_solution_metrics.path_lengths,
	Solution_Heights = Root_To_Solution_Metrics#root_to_solution_metrics.solution_heights,
	New_Intermediate_Metrics = #root_to_solution_metrics{path_lengths=[Path_Length|Path_Lengths],
													 solution_heights=[Height|Solution_Heights]},
	traverse_tree(Tree_Nodes,Heights,New_Intermediate_Metrics,Path_Length+1);

traverse_tree([Tree_Node|Tree_Nodes],[_Height|Heights],Root_To_Solution_Metrics,Path_Length) when Tree_Node#tree_node.satisfiable =:= false ->
	traverse_tree(Tree_Nodes,Heights,Root_To_Solution_Metrics,Path_Length+1);

traverse_tree([Tree_Node|Tree_Nodes],[Height|Heights],Root_To_Solution_Metrics,Path_Length) ->
	Sibling_Nodes = tree_persist:get_node_children(Tree_Node),
	Left_Sibling = Sibling_Nodes#sibling_nodes.left_sibling,
	Right_Sibling = Sibling_Nodes#sibling_nodes.right_sibling,
	traverse_tree([Left_Sibling,Right_Sibling|Tree_Nodes],[Height+1,Height+1|Heights],Root_To_Solution_Metrics,Path_Length+1).

%%
%%	Public helper function for printing #root_to_solution_metrics to io_device()
%%
%%	IO_Device: The io_device() to which we will print
%%		io_device()
%%	Tree_Metric: The metric which will be printed
%%		#tree_metric
%%
print_metric(IO_Device,Tree_Metric) ->
    Root_To_Solution_Metrics = Tree_Metric#tree_metric.metric_data,
	Path_Lengths = Root_To_Solution_Metrics#root_to_solution_metrics.path_lengths,
	Path_Lengths_Column = ["Path_Lengths"|Path_Lengths],
    Path_Intervals = util:difference_list([0|Path_Lengths]),
	Path_Intervals_Column = ["Path_Intervals"|Path_Intervals],
	Solution_Heights = Root_To_Solution_Metrics#root_to_solution_metrics.solution_heights,
	Solution_Heights_Column = ["Solution_Heights"|Solution_Heights],
	metric_util:r_format_lists([Path_Lengths_Column,Path_Intervals_Column,Solution_Heights_Column],IO_Device).
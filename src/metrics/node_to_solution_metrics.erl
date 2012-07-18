%% Author: Francis Stephens
%% Created: 19 Sep 2008
%% Description: TODO: Add description to solution_distance
-module(node_to_solution_metrics).

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
-define(METRIC_TYPE,"Node To Solution Metrics").

%%
%%	Records
%%

%%
%%	A #node_to_solution_metrics contains two dictionaries mapping tree levels (sets of nodes at a particular height w.r.t. the root)
%%	to a particular metric.
%%
%%	The first dictionary, level_depths, maps the depth of the nearest solution w.r.t. the node being measured at that level. The depth 
%%	of a node below a particular node is just the difference between their respective levels.  For example a solution occurring at level
%%	5 is at depth 3 w.r.t. an ancestor node  at level 2 and is at depth 5 w.r.t. the root it is also at depth 0 w.r.t. itself.  A node,
%%	X, is not considered to be at any depth below another node, Y, if Y is not an ancestor of X.
%%
%%	The second dictionary, level_distances, maps the distance travelled (in a top-down left to right fashion) to the nearest solution
%%	w.r.t. the node being measured at that level.  The distance from a node, X, to another node, Y, (again X must be an ancestor of Y)
%%	is the number of edges traversed to arrive at that node following a depth-first (top-down left to right fashion) approach to traversing
%%	the tree.  This description is quite vague and it is assumed that the reader have a good intuitive idea of the depth-first search described.
%%	This metric has the interesting property that if left sub-tree of a node contains a solution then the distance from that node to the nearest
%%	solution as described will always just be one more than it's left child's distance.
%%
%%	level_heights: A dictionary mapping levels of the tree (Integer()) to lists of 'nearest solution' distances ([Integer()])
%%		Dict()
%%
-record(node_to_solution_metrics,{level_depths,level_distances}).

%%
%% Exported Functions
%%
-export([compute_metric/1]).

%%
%% API Functions
%%

%%
%%	This function computes two metrics made by mapping tree levels (sets of nodes at a particular height w.r.t. the root)
%%	to both the depth of the nearest solution below a node and the nearest depth-first travelling distance to a solution node.
%%	For details see #node_to_solution_metrics above.
%%
%%	Tree_Id: The unique id of a search tree in the database, this is the tree for which these metrics will be computed
%%		Integer()
%%
compute_metric(Tree_Id) -> 
    Tree_Root = tree_persist:get_tree_root(Tree_Id),
	{Depth_Dict,_Depth,Dist_Dict,_Dist} = traverse_tree(Tree_Root),
	Metric_Data = #node_to_solution_metrics{level_depths=Depth_Dict,level_distances=Dist_Dict},
	#tree_metric{metric_type=?METRIC_TYPE,tree_id=Tree_Id,print_metric=fun print_metric/2,metric_data=Metric_Data}.

%%
%% Local Functions
%%

traverse_tree(Tree_Root) ->
	Sibling_Nodes = tree_persist:get_root_children(Tree_Root),
	Left_Sibling = Sibling_Nodes#sibling_nodes.left_sibling,
	Right_Sibling = Sibling_Nodes#sibling_nodes.right_sibling,
	{Left_Depth_Dict,Left_Depth,Left_Dist_Dict,Left_Dist} = traverse_tree(Left_Sibling,2,2),
	{Right_Depth_Dict,Right_Depth,Right_Dist_Dict,Right_Dist} = traverse_tree(Right_Sibling,2,2),
	manage_dicts(1,1,Left_Depth,Right_Depth,Left_Dist,Right_Dist,Left_Depth_Dict,Right_Depth_Dict,Left_Dist_Dict,Right_Dist_Dict).

traverse_tree(Tree_Node,Height,Dist) when Tree_Node#tree_node.satisfiable =:= true ->
	{dict:store(Height,[0],dict:new()),1,dict:store(Height,[0],dict:new()),Dist};

traverse_tree(Tree_Node,_Height,Dist) when Tree_Node#tree_node.satisfiable =:= false -> 
	{dict:new(),infinity,dict:new(),Dist};

traverse_tree(Tree_Node,Height,Dist) ->
	Sibling_Nodes = tree_persist:get_node_children(Tree_Node),
	Left_Sibling = Sibling_Nodes#sibling_nodes.left_sibling,
	Right_Sibling = Sibling_Nodes#sibling_nodes.right_sibling,
	traverse_and_record(Left_Sibling,Right_Sibling,Height,Dist).

%% This is all fucked by the presence of uni-branching nodes which leave behind their devil spawn undefined child-nodes!
traverse_and_record(undefined,Right_Sibling,Height,Dist) ->
	{Right_Depth_Dict,Right_Depth,Right_Dist_Dict,Right_Dist} = traverse_tree(Right_Sibling,Height+1,Dist+1),
	manage_dicts(Height,Dist,Left_Depth,Right_Depth,Left_Dist,Right_Dist,Left_Depth_Dict,Right_Depth_Dict,Left_Dist_Dict,Right_Dist_Dict);

%% This is all fucked by the presence of uni-branching nodes which leave behind their devil spawn undefined child-nodes!    
traverse_and_record(Left_Sibling,undefined,Height,Dist) ->
	{Right_Depth_Dict,Right_Depth,Right_Dist_Dict,Right_Dist} = traverse_tree(Left_Sibling,Height+1,Dist+1),
	manage_dicts(Height,Dist,Left_Depth,Right_Depth,Left_Dist,Right_Dist,Left_Depth_Dict,Right_Depth_Dict,Left_Dist_Dict,Right_Dist_Dict);

traverse_and_record(Left_Sibling,Right_Sibling,Height,Dist) ->
    {Left_Depth_Dict,Left_Depth,Left_Dist_Dict,Left_Dist} = traverse_tree(Left_Sibling,Height+1,Dist+1),
	{Right_Depth_Dict,Right_Depth,Right_Dist_Dict,Right_Dist} = traverse_tree(Right_Sibling,Height+1,Left_Dist+1),
	manage_dicts(Height,Dist,Left_Depth,Right_Depth,Left_Dist,Right_Dist,Left_Depth_Dict,Right_Depth_Dict,Left_Dist_Dict,Right_Dist_Dict).

%%
%%	This function produces new dictionaries using the existing dictionaries and the depth and distance information
%%	available in this part of the search tree.
%%
%%	There are two numeric values calculated and returned alongside the dictionaries.
%%		Min_Depth is the shortest path-length (in terms of edges traversed) from the current node to any node below.
%%			NB: If there are no solutions below this node then Min_Depth = infinity
%%		Min_Depth+1 is the shortest path-length from the parent of the current node to a solution below the current node.
%%		Usable_Dist is the left-first downwards distance (in terms of edges traversed) from the root node to the left-most solution below.
%%			NB: If there are no solutions below this node then Usable_Dist = infinity.
%%
%%	Note on distance measurement:
%%		When calculating Usable_Dist the 'total distance travelled' to the left-most solution below is used and the 'total distance travelled'
%%		to the current node is subtracted from this total to provide the distance from the current node to the left-most solution below.  It is
%%		important to note that the 'total distance travelled' is not really the total distance.  The only edges that are counted are those 
%%		toward the 'total distance travelled' are those which have a solution below.  The count appears to be traversing (left-first downward)
%%		the search tree with all non-solution baring subtrees removed.
%%
%%	There are two dictionaries:
%%		Depth_Dict maps the Min_Depth to the current node's distance from the root
%%		Dist_Dict records the left-first downwards distance (in terms of edges traversed) from the current node to the left-most solution below.
%%
%%	Height: The height (shortest path-length from the root) of the current node.
%%		Integer()
%%	Dist: The distance travelled (left-first downwards) to the current node.  Read "Note on distance measurement" above.
%%		Integer()
%%	Left_Depth: The shortest path-length to a solution below the left child of the current node.
%%		Integer() | infinity
%%	Right_Depth: The shortest path-length to a solution below the right child of the current node.
%%		Integer() | infinity
%%	Left_Dist: The distance travelled (left-first downward) to the left-most solution below the left child of the current node.
%%			   Read "Note on distance measurement" above.
%%		Integer() | infinity
%%	Right_Dist: The distance travelled (left-first downward) to the left-most solution below the right child of the current node.
%%				Read "Note on distance measurement" above.
%%		Integer() | infinity
%%	Left_Depth_Dict: Depth_Dict from the left subtree of the current node.
%%		dict() maps Integer -> [Integer()]
%%	Right_Depth_Dict: Depth_Dict from the right subtree of the current node.
%%		dict() maps Integer -> [Integer()]
%%	Left_Dist_Dict: Dist_Dict from the left subtree of the current node.
%%		dict() maps Integer -> [Integer()]
%%	Right_Dist_Dict: Depth_Dict from the right subtree of the current node.
%%		dict() maps Integer -> [Integer()]
%%
manage_dicts(Height,Dist,Left_Depth,Right_Depth,Left_Dist,Right_Dist,Left_Depth_Dict,Right_Depth_Dict,Left_Dist_Dict,Right_Dist_Dict) ->
	Min_Depth = stats_util:min(Left_Depth,Right_Depth),
	%% The two lines below guarantee that if there is a solution to the left we will use the left distance otherwise we will use the right distance
	%% They rely on distances on branches without solutions being infinite
	Left_Dist_Ind = stats_util:max(Left_Dist,Left_Depth),
	Usable_Dist = stats_util:min(Left_Dist_Ind,Right_Dist),
	Merge_Fun = fun(_K,V1,V2)->V1++V2 end,
	Merge_Depth_Dict = dict:merge(Merge_Fun,Left_Depth_Dict,Right_Depth_Dict),
	Merge_Dist_Dict = dict:merge(Merge_Fun,Left_Dist_Dict,Right_Dist_Dict),
	Depth_Dict = dict:append(Height,Min_Depth,Merge_Depth_Dict),
	Dist_Dict = dict:append(Height,calc_distance(Usable_Dist,Dist),Merge_Dist_Dict),
	case Min_Depth of
		infinity
			->	{Depth_Dict,infinity,Dist_Dict,Usable_Dist};
		_Else
			->	{Depth_Dict,Min_Depth+1,Dist_Dict,Usable_Dist}
	end.

%%
%%	Calculates the distance (left-first downwards) from the current node to the left-most solution
%%	below by subtracting the total distance to that solution from the distance travelled to this node.
%%	Note that infinity minus some number is still considered infinity.
%%
%%	Total_Dist: The total distance from the root to the left-most solution below the current node.
%%		Integer() | infinity
%%	Dist_To: The distance travelled (left-first downwards) to the current node.
%%			 Note that this is not necessarily the shortest path-length from the root as a direct route may not have been taken.
%%		Integer()
%%
calc_distance(inifinity,_Dist_To) ->
	infinity;

calc_distance(Total_Dist,Dist_To) ->
	Total_Dist - Dist_To.

%%
%%	Helper function for printing #node_to_solution_metrics to io_device()
%%
%%	IO_Device: The io_device() to which we will print
%%		io_device()
%%	Tree_Metric: The metric which will be printed
%%		#tree_metric
%%
print_metric(IO_Device,Tree_Metric) ->
    Node_To_Solution_Metrics = Tree_Metric#tree_metric.metric_data,
	Level_Distances = dict:to_list(Node_To_Solution_Metrics#node_to_solution_metrics.level_distances),
	Level_Distances_Column = ["Level_Distances"|Level_Distances],
	Level_Depths = dict:to_list(Node_To_Solution_Metrics#node_to_solution_metrics.level_depths),
	Level_Depths_Column = ["Level_Depths"|Level_Depths],
	metric_util:r_format_lists([Level_Distances_Column,Level_Depths_Column],IO_Device).
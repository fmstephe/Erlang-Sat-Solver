%% Author: Francis Stephens
%% Created: 16 Jun 2008
%% Description: TODO: Add description to process_records

%%
%% Records
%%

%%%
%%%	A #coop_info contains the information needed for delegating search tree branches
%%%	and aggregating search tree nodes from a sat search tree traversal.
%%%
%%%	branch_name: The unique name given to this traversing process (not the process id).
%%%				 branch_name is required to be unique w.r.t. each of the processes working on a particular tree.	
%%%		String()
%%%	delegator_id: The process id for the delegating server.
%%%		Pid()
%%%	aggregator_id: The proces id for the aggregating server.
%%%		Pid()
%%%
-record(coop_info,{branch_name,delegator_id,aggregator_id}).

%%%
%%%	A #tree_counter maintains a persistent counter for providing unique identifiers
%%%	for search trees.  This enables the decoupling of a tree's id from the contents
%%%	of the search tree database.  It is very likely that the search tree database will
%%%	be purged between runs but the unique id of the tree will be persisted for longer
%%%	in the statistics that will be saved to the file-system.
%%%
%%%	counter_name: The name of the counter so we can maintain seperate counters
%%%				  There is a default counter used to count search trees called 'tree_counter'
%%%		Atom()
%%%	count: The current count for this counter
%%%		Integer()
%%%
-record(counter,{counter_name,count}).

%%%
%%%	A #tree_root is a message record for sending the root node of a new tree to a tree_aggregating process
%%%
%%%	tree_id: A unique identifier for this node in the database
%%%		Integer()
%%%	root_name: The name of the root node of a search tree.
%%%		String()
%%%	tree_metadata: Arbitrary meta-data about this tree
%%%		Term()
%%%
-record(tree_root,{tree_id,root_name,satisfiable,tree_metadata}).

%%%
%%%	A #tree_node is a message record for sending a new tree node to a tree_aggregating process
%%%
%%%	node_id: A unique complex identifier for a tree_node
%%%		#node_id
%%%	parent_name: The name of the parent of this node (Required in order to attach this node to its parent)
%%%		String()
%%%	left_or_right: Indicates whether this tree_node is the left or right handed child of its parent
%%%				   There is a special case where a formula is determined to be satisfiable or unsatisfiable
%%%				   before the split phase has occurred.  In this case the search tree proceeds from the root to a single
%%%				   node and is unbranched.
%%%		left | right | unbranched
%%%	Satisfiable: Ternary value indicating whether or not this node is satisfied (true), unsatisfiable (false) or indetermined
%%%		true | false | indetermined
%%%	variable_assignment: The variable assignment which produced this tree node
%%%		#variable_assignment
%%%
-record(tree_node,{node_id,parent_name,left_or_right,satisfiable,variable_assignment}).

%%%
%%%	A #node_id uniquely identifies a particular tree node provided the following is true
%%%		1: The node_name is unique w.r.t. the tree this node appears in
%%%		2: The tree_id is unique w.r.t. every tree in the database
%%%
%%%	node_name: The name of the node to be aggregated.
%%%		String()
%%%	tree_id: The id (unique w.r.t. all trees in the database) of the tree in which this node appears
%%%		Integer()
%%%
-record(node_id,{tree_id,node_name}).

%%%
%%%	A #sibling_nodes identifies two #tree_node records where they both share the same parent node
%%%	Left_Sibling#tree_node.parent_name = Right_Sibling#tree_node.parent_name
%%%
%%%	left_sibling: The left-handed child node - Left_Sibling#tree_node.left_or_right = left
%%%		#tree_node
%%%	right_sibling: The right-handed child node - Right_Sibling#tree_node.left_or_right = right
%%%		#tree_node
%%%
-record(sibling_nodes,{left_sibling,right_sibling}).

%%%
%%%	A #create_tree is used as a cast to a tree_agg_callback server to create a new tree in the database
%%%
%%%	root_name: The name of the root of the new tree (Usually "Root").
%%%		String()
%%%	satisfiable: Ternary value indicating whether or not the root of this tree is satisfied (true), unsatisfiable (false) or indetermined
%%%		true | false | indetermined
%%%	persistent_metadata: Arbitrary metadata that is to be persisted along-side the search tree
%%%		Dict()
-record(create_tree,{root_name,satisfiable,persistent_metadata}).

%%%
%%%	An #attach_tree_metadata is used to attach meta-data to an existing search tree in the database.
%%%
%%%	tree_id: The unique id of the tree to which this metadata will be attached
%%%		Integer()
%%%	tree_metadata: The meta-data to be attached
%%%		Term()
%%%
-record(attach_tree_metadata,{tree_id,tree_metadata}).

%%%
%%%	A #tree_metadata is contains metadata which describes which formula was searched and
%%%	which strategy was used to search it.
%%%
%%%	generated_formula: A #formula_def and creational metadata
%%%		#generated_formula
%%%	strategy_name: The name of the strategy (see #dp_funs) used to search the formula
%%%		Atom()
%%%
-record(tree_metadata,{generated_formula,strategy_name}).

%%%
%%%	A #branch_name contains the unique name for a SAT process which has been delegated a branch to search
%%%
%%%	name: The name of the branch
%%%		String()
%%%
-record(branch_name,{name}).

%%%
%%%	A tree_metric contains the result of procesing a particular tree using a particular metric.
%%%
%%%	metric_type: The name of the metric used to produce metric_data.  Ideally metric_type should uniquely
%%%				 identify a single metric algorithm.
%%%		#String()
%%%	tree_id: A foreign key to a single tree_root record in the database.
%%%		#Integer()
%%%	print_metric: A helper function for printing the metric in a friendly way to some io_device()
%%%				  Must take an io_device() and #tree_metric as arguments reader friendly string is then
%%%				  printed to the io_device() provided.  NB: The Atom standard_io indicates that the console
%%%				  is to be used for printing.  The function must return the Atom ok if the write proceeded
%%%				  without error or {error,Reason} otherwise.
%%%		fun()/2
%%%	metric_data: Arbitrary result data from the metric applied
%%%		Record()
%%%
-record(tree_metric,{metric_type,tree_id,print_metric,metric_data}).
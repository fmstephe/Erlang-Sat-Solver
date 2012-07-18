%% Author: Francis Stephens
%% Created: 27 Jun 2008
%% Description: It is important to note that this module does not support concurrent access to the tree database.
%%				All access must be made from a single process (or all hell will break loose)
-module(tree_persist).

%%
%% Include files
%%

-include("process_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%
%%	Macros
%%

-define(TREE_COUNTER,tree_counter).

%%
%%	Records
%%

%%
%% Exported Functions
%%
-export([init_database/0,clear_trees/0,clear_trees_and_counter/0,get_tree_node/1,set_tree_node/1,create_tree/3,get_tree_root/1,get_all_tree_roots/0,get_all_tree_ids/0,get_node_children/1,get_root_children/1,attach_tree_metadata/1]).

%%
%% API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Creational Functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns ok or {error,Reason}.
%%	Creates a new Mnesia database which can be used to store and retrieve test data-sets.
%%	This function creates a singleton database which is used centrally via this module.  
%%	The database will create a disc_node for tree counters on the local node and thus persist
%%	the tree count between test runs.  If a schema containing the expected table already exists
%%	this funtion will start mnesia if it is not already running.
%%
init_database() -> 
	Create_Schema = fun() ->
						persistence_util:create_schema()
					end,
    Start_Mnesia = 	fun() ->
                   		persistence_util:start_mnesia()
					end,
	Create_Tables = fun() ->
						persistence_util:create_table(tree_root,record_info(fields,tree_root),[]),
						persistence_util:create_table(tree_node,record_info(fields,tree_node),[]),
						persistence_util:create_table(counter,record_info(fields,counter),[{disc_copies,[node()]}])
					end,
	lists:foreach(fun(Fun)->Fun()end,[Create_Schema,Start_Mnesia,Create_Tables]).

%%
%%	Returns ok or {error,Reason}.
%%	Clears all existing search tree data out of the mnesia database.  This does not delete the
%%	counter currently generating tree ids.  The schemas will still exist (if they already existed)
%%	and mnesia will still be running (if it was already running).
%%
clear_trees() ->
	mnesia:clear_table(tree_root),
	mnesia:clear_table(tree_node).

%%
%%	Returns ok or {error,Reason}.
%%	Deletes the tree counter from the database, effectively setting it back to zero.
%%	Clears all existing search tree data out of the mnesia database.  This does not delete the
%%	counter currently generating tree ids.  The schemas will still exist (if they already existed)
%%	and mnesia will still be running (if it was already running).
%%
clear_trees_and_counter() ->
	mnesia:dirty_delete(counter,?TREE_COUNTER),
	clear_trees().

%%******************************%%
%%	Local Creational functions	%%
%%******************************%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Enquiry Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns the #persistent_test_formulae associated with the formula_key provided or the atom
%%	empty.
%%
%%	Node_Id: The complex unique identifier for a tree node in the database
%%		#node_id
%%
get_tree_node(Node_Id) ->
	R = fun() ->
        mnesia:read({tree_node,Node_Id})
		end,
	{atomic,Val} = mnesia:transaction(R),
	case length(Val) of
		0
			->	empty;
		1
			->	[Tree_Node|[]] = Val,
				Tree_Node
	end.

%%
%%	Returns the #tree_root associated with the Tree_Id provided or the atom empty if no tree
%%	exists in the database with that id.
%%
%%	Tree_Id: The unique id for a #tree_root in the database
%%		Integer()
%%
get_tree_root(Tree_Id) ->
	R = fun() ->
        mnesia:read({tree_root,Tree_Id})
		end,
	{atomic,Val} = mnesia:transaction(R),
	case length(Val) of
		0
			->	empty;
		1
			->	[Tree_Root|[]] = Val,
				Tree_Root
	end.

%%
%%	Returns all tree roots (#tree_root) currently in the database.
%%
get_all_tree_roots() ->
	Q_All = qlc:q([Tree_Root||Tree_Root<-mnesia:table(tree_root)]),
	R = fun() ->
        qlc:e(Q_All)
		end,
	{atomic,Val} = mnesia:transaction(R),
	Val.

%%
%%	Returns each id (Integer()) for all tree roots (#tree_root) currently in the database.
%%
get_all_tree_ids() ->
	Q_Id = qlc:q([Tree_Id||{tree_root,Tree_Id,_Root_Name}<-mnesia:table(tree_root)]),
	R = fun() ->
        qlc:e(Q_Id)
		end,
	{atomic,Val} = mnesia:transaction(R),
	Val.

%%
%%	Returns a #sibling_nodes if the node indicated by the #tree_root provided has children in the database
%%	or empty otherwise.
%%
%%	Tree_Root: The root whose children we seek.
%%		#tree_root
%%
get_root_children(Tree_Root) ->
	Tree_Id = Tree_Root#tree_root.tree_id,
	Root_Name = Tree_Root#tree_root.root_name,
	get_children(Tree_Id,Root_Name).

%%
%%	Returns a #sibling_nodes if the node indicated by the #tree_node provided has children in the database
%%	or empty otherwise.
%%
%%	Tree_Node: The node whose children we seek.
%%		#tree_node
%%
get_node_children(Tree_Node) ->
	Tree_Id = (Tree_Node#tree_node.node_id)#node_id.tree_id,
	Node_Name = (Tree_Node#tree_node.node_id)#node_id.node_name,
	get_children(Tree_Id,Node_Name).

attach_tree_metadata(Attach_Tree_Metadata) ->
	Tree_Id = Attach_Tree_Metadata#attach_tree_metadata.tree_id,
	Tree_Metadata = Attach_Tree_Metadata#attach_tree_metadata.tree_metadata,
	Tree_Root = get_tree_root(Tree_Id),
	New_Tree_Root = #tree_root{ tree_id=Tree_Id,
								root_name=Tree_Root#tree_root.root_name,
								satisfiable=Tree_Root#tree_root.satisfiable,
								tree_metadata=Tree_Metadata},
    write_tuple(New_Tree_Root).

%%******************************%%
%%	Local Query functions		%%
%%******************************%%


%%
%%	Returns a #sibling_nodes if the node indicated by the #tree_root provided has children in the database
%%	or empty otherwise.
%%
%%	Tree_Id: The unique id of the tree in which we are search for children nodes
%%		Integer()
%%	Parent_Name: The name of the node (unique w.r.t. this tree) whose children we seek
%%	String()
%%
get_children(Tree_Id,Parent_Name) ->
	Q_Id = qlc:q([Tree_Node||Tree_Node<-mnesia:table(tree_node),
								(Tree_Node#tree_node.node_id)#node_id.tree_id =:= Tree_Id,
								Tree_Node#tree_node.parent_name =:= Parent_Name]),
	R = fun() ->
        qlc:e(Q_Id)
		end,
	case mnesia:transaction(R) of
		{atomic,[]}
			->	empty;
		{atomic,[Child]}
			->	make_children_tuple(Child);
		{atomic,[Child_1,Child_2]}
			->	make_children_tuple(Child_1,Child_2)
	end.

%%
%%	Returns a #sibling_nodes provided the two #tree_nodes are in fact siblings.
%%	The #tree_nodes provided must share a common parent node and one must be left-handed
%%	while the other is right-handed.
%%	This function also handles the single-child case and returns a #sibling_nodes with an
%%	appropriately empty left or right sibling in this case.
%%
%%	Node_1: The first of two #tree_nodes
%%		#tree_node
%%	Node_2: The second of two #tree_nodes
%%		#tree_node
%%
make_children_tuple(Child_1,Child_2) ->
    case Child_1#tree_node.left_or_right of
        left
        	->	#sibling_nodes{left_sibling=Child_1,right_sibling=Child_2};
        right
        	->	#sibling_nodes{left_sibling=Child_2,right_sibling=Child_1}
    end.

make_children_tuple(Child) ->
    case Child#tree_node.left_or_right of
        left
        	->	#sibling_nodes{left_sibling=Child};
    	right
			->	#sibling_nodes{right_sibling=Child}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Modifying Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Writes the #tree_node provided to the database
%%
set_tree_node(Tree_Node) when is_record(Tree_Node,tree_node) ->
    write_tuple(Tree_Node).

%%
%%	Returns a new #tree_root whose name is unque w.r.t. every other tree in the database.
%%
create_tree(Root_Name,Satisfiable,Creational_Metadata) ->
	Tree_Id = incr_counter(?TREE_COUNTER),
	create_tree_root(Root_Name,Satisfiable,Creational_Metadata,Tree_Id).

%%******************************%%
%%	Local Modifying functions	%%
%%******************************%%

%%
%%	Writes the provided tuple to mnesia in a dirty fashion.
%%	The table in which the tuple is stored is defined as the value (atom())
%%	of the first element of the tuple.
%%
%%		Tuple
%%		{table_name,key,val1,...,valn}
%%
write_tuple(Tuple) ->
    mnesia:dirty_write(Tuple).

%%
%%	Returns the current value of the named counter in the database.  If the named
%%	counted does not exist in the database it will be created and 0 returned as it's
%%	'current' value. As a side-effect the named counter is incremented in the database
%%	Like this entire module this is not safe if the database if accessed concurrently.
%%
%%	Counter_Name: The name of the counter to be incremented
%%		String()
%%
incr_counter(Counter_Name) ->
	Count = (get_counter(Counter_Name))#counter.count,
	mnesia:dirty_write(#counter{counter_name=Counter_Name,count=Count+1}),
	Count.

%%
%%	Returns the named #counter as it exists in the database.  If the named
%%	counter does not exist it will be created with initial count 0.
%%
%%	Counter_Name: The name of the counter to be retrieved (or created)
%%		String()
%%
get_counter(Counter_Name) ->
	Counter = mnesia:dirty_read(counter,Counter_Name),
	case Counter of
		[]
			->	
				New_Counter = #counter{counter_name=Counter_Name,count=0},
				mnesia:dirty_write(New_Counter),
				New_Counter;
		[Single_Counter]
			->	Single_Counter
	end.

%%
%%	Creates a guaranteed unique id for a new tree.
%%	The use of transactions in this function is very important for guaranteeing
%%	the uniqueness of the id created.
%%
create_tree_root(Root_Name,Satisfiable,Tree_Metadata,Tree_Id) ->
	Tree_Root = #tree_root{tree_id=Tree_Id,root_name=Root_Name,satisfiable=Satisfiable,tree_metadata=Tree_Metadata},
	write_tuple(Tree_Root),
	Tree_Root.

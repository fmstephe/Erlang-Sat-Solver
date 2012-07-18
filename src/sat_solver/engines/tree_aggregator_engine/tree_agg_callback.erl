%%% -------------------------------------------------------------------
%%% Author  : Francis Stephens
%%% Description :
%%%
%%% Created : 22 Jun 2008
%%% -------------------------------------------------------------------
-module(tree_agg_callback).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("process_records.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%
%%%	The state of a tree_agg_callback process is the name of the database
%%%	in which the search tree data is stored and the satisfiability of the
%%%	last sat instance run.
%%%
%%%	The satisfiability state means that we can only ever
%%%	run one sat instance at a time through a particular tree_agg_callback
%%%	server while still expecting the satisfiability state to remain meaningful.
%%%
%%%	There is also a synchronisation problem w.r.t. the satisfiability state
%%%	because I don't think that we can guarantee that a query to this state,
%%%	even made after the termination of a sat instance, will represent the final
%%%	satisfiability state of that instance.  The degenerative case exists where
%%%	the state is set to 'satisfiable' for the first time very near to the end 
%%%	of the termination of the sat instance and the query of the satisfiability
%%%	state is made before the satisfiable cast is processed.  This is very unlikely
%%%	and at least for now I will just live with it.  If test cases start to fail then
%%%	I will revisit this problem.
%%%
%%%	Update: I don't think this problem actually exists.  Luckily Erlang enforces message
%%%			ordering and so provided I am:
%%%								1) Only solve one formula at a time and,
%%%								2) Send the satisfiability query message from the same process that actually solved
%%%								   the formula.
%%%			we won't have any problems :) (maybe)
%%%
%%%		database_name: The name of the database to which search tree data is stored.
%%%			string()
%%%		satisfiable: Indicates whether the currently processed sat instance is satisfiable
%%%			true | false | indetermined
%%%		last_tree_id: The unique ID of the last tree that was created in the database, will be negative if not tree has been created
%%%			Integer()
%%%
-record(state,{satisfiable,last_tree_id}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server TODO Pretty sure Db_Name is completely obsolete.
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    State = init_state(),
    tree_persist:init_database(),
    {ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(satisfiable,_From,State) ->
    Reply = State#state.satisfiable,
    {reply,Reply,State};

handle_call(last_tree_id,_From,State) ->
    Reply = State#state.last_tree_id,
    {reply,Reply,State};

handle_call(Attach_Tree_Metadata,_From,State) when is_record(Attach_Tree_Metadata,attach_tree_metadata) ->
	Reply = tree_persist:attach_tree_metadata(Attach_Tree_Metadata),
    {reply,Reply,State};

handle_call(Create_Tree,_From,_State) when is_record(Create_Tree,create_tree) ->
    Root_Name = Create_Tree#create_tree.root_name,
	Satisfiable = Create_Tree#create_tree.satisfiable,
    Persistent_Metadata = Create_Tree#create_tree.persistent_metadata,
    New_Tree = tree_persist:create_tree(Root_Name,Satisfiable,Persistent_Metadata),
	{reply,New_Tree,#state{satisfiable=Satisfiable,last_tree_id=New_Tree#tree_root.tree_id}}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%%
%%		NB:	It is probably not a good idea to link server behaviour directly
%%			to such low level records like tree_node and tree_root.
%%
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Tree_Node,State) when is_record(Tree_Node,tree_node) ->
    tree_persist:set_tree_node(Tree_Node),
    {noreply, next_state(State,Tree_Node#tree_node.satisfiable)}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%%
%%	Changes the satisfiability state.
%%	indetermined is replaced with any state
%%	false can only be replaced by true
%%	true is never replaced
%%
next_state({state,indetermined,Last_Tree_Id},Satisfiable) -> 
	#state{satisfiable=Satisfiable,last_tree_id=Last_Tree_Id};

next_state({state,false,Last_Tree_Id},true) ->
	#state{satisfiable=true,last_tree_id=Last_Tree_Id};

next_state(State,_Satisfiable) ->
	State.

%%
%%	Resets the satisfiability state to indetermined
%%
init_state() ->
	#state{satisfiable=indetermined,last_tree_id=-1}.
%% Author: Francis Stephens
%% Created: 1 Dec 2007
%% Description: Erlang Header file defining useful common records for communicating sat solver modules

%%
%% Records
%%

%%%
%%%	A #generated_formula contains both a formula definition and a dictionary containing key-value
%%%	pairs describing the creational properties of the formula.  This record looks very similar
%%%	to sat_state below.  However, while sat_state is intended to be used to describe the state
%%%	of a formula while it is being solved formula_and_metadata is used for describing the creational
%%%	properties of a formula.
%%%	
%%%	For example formula_metadata might contain the following key-value pairs:
%%%		var_num = 20, clause_num = 5, clause_size = 3
%%%
%%%	formula_def: The formula whose satisfiability is being determined
%%%		#formula_def
%%%	creational_metadata: Key-value pairs describing the parameters used to create this formula
%%%		Dict()
%%%	print_generated_formula: A tuple containing a module and function name, formatted as {module,function}
%%%					for printing the generated formula in a friendly way to some io_device().  Must take 
%%%					an io_device() and a #generated_formula.  A friendly string is then printed to the 
%%%					io_device() provided.  NB: The Atom standard_io indicates that the console is to be 
%%%					used for printing.  The function must return the Atom ok if the write proceeded without
%%%					error or {error,Reason} otherwise.
%%%		{Atom(),Atom()}
%%%
-record(generated_formula,{formula_def,creational_metadata,print_generated_formula}).

%%%
%%%	A sat_state contains both a formula and arbitrary metadata describing aspects of the formula.
%%%	It is intended that the metadata be both set and read by the sat algorithms contained in a
%%%	#dp_funs record which is currently being used to determine the satisfiability of the formula.
%%%
%%%	formula: The formula whose satisfiability is being determined
%%%		#formula
%%%	state_metadata: Arbitrary metadata describing aspects of formula
%%%		Term()
%%%	
-record(sat_state,{formula,state_metadata}).

%%%
%%%	A sat_result contains the conclusion of a SAT algorithm (or smaller function) performed on a particular formula
%%%	A sat_result is either satisfied (true) unsatisfiable (false) or not yet determined (indetermined)
%%%	A sat_result also records the variable assignments for the given formula that corresponds to the result state (satisfied)
%%%
%%%	satisfiable
%%%		true | false | indetermined
%%%	formula: May be null under some circumstances, for example when a formula is unsatisfiable and we can't produce an
%%%			 assignment which demonstrates that this is true
%%%		#sat_state | null
-record(sat_result,{satisfiable, sat_state}).

%%%
%%%	a dp_funs contains the functions that are required to perform a Davis Putnam SAT search using a dp_engine
%%%
%%%	strategy_name: The name of the search strategy made up by this collection of functions
%%%		Atom()
%%%	pre_funs: List of functions, each function taking a single #sat_state as argument and returning a #sat_result
%%%		[fun/1]
%%%	branch_fun: A single function which takes the current workstack ([#sat_state]) and returns a new one,
%%%				presumably the usual searching will be done by this function.
%%%				The function is free to assume that it will never be passed an empty workstack.
%%%		fun/1
%%%	post_funs: List of functions, each function taking a single #sat_state as argument and returning a #sat_result
%%%		[fun/1]
%%%	init_sat_state: Function taking a single #formula_def as an argument and returning a #sat_state
%%%		fun/1
%%%	engine_args: Additional arbitrary arguments supplied for a specific solver engine
%%%		Term()
%%%
-record(dp_funs,{strategy_name,pre_funs,branch_fun,post_funs,init_sat_state,engine_args}).

%%%
%%%	A variable_assignment contains the name of the retrieved literal plus the value it will have on the left branch
%%%	l_name
%%%		atom()
%%%	value
%%%		true | false
%%%
-record(variable_assignment,{l_name, value}).

%%
%%	A #literal_count indicates the number of positive and negative literals appearing for a
%%	particular variable in a cnf_graph formula.
%%
%%	variable_name: The name of the variable being profiled
%%		Atom()
%%	positive_count: The number of positive literals with variable_name appearing in the formula
%%		Integer()
%%	negative_count: The number negative literals with variable_name appearing in the formula
%%		Integer()
%%
-record(literal_count,{variable_name,positive_count,negative_count}).
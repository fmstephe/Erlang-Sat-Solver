%% Author: Francis Stephens
%% Created: 25 Feb 2008
%% Description: TODO: Add desciption to cnf_formula
-module(cnf_graph).

%%
%%	Include files
%%

-include("sat_records.hrl").
-include("test_records.hrl").
-include("cnf_formula_records.hrl").

%%
%%	Records
%%

%%
%%	Macros
%%

%%
%%	The core data-structure is a dictionary mapping every variable to a list of connections.  Every
%%	connection is a directed edge in a graph connecting two variables in the formula.  Two variables
%%	will be connected if they appear in a clause together, if they appear in many clauses together
%%	they will have many connections.  A variable, k, then acts as a key to a list of every other variable
%%	appearing in a clause with k.
%%
%%	connection_dict: The dictionary mapping variables to lists of #connection records
%%		dict(): Atom() -> [#connection]
%%	variable_dict: The dictionary mapping variables to their assigned values
%%		dict(): Atom() -> (true | false | indetermined)
%%	satisfiable: A flag indicating whether the formula, with given variable assignments, has been determined
%%				 to be satisfiable
%%		true | false | indetermined
%%
-record(cnf_graph,{connection_dict,variable_dict,satisfiable}).

%%
%%	A #connection records a connection from a variable to another variable in the formula.
%%	There are, of course, two variables involved in a connection between two variables but only
%%	one is recorded in this record.
%%	
%%	A #cnf_graph contains a dictionary mapping between variable names and
%%	lists of #connection records.  The variable acting as a key, k, to a list of #connections is
%%	implicitly connected to the variable, v, connected by a clause in the formula.  Standard graph
%%	theory would write this directed edge as e(k,v), meaning the edge runs from k to v.  Since 
%%	edges in this cnf_graph are also labeled with the negation of the variable k and an id identifying
%%	the clause where this connection is made we might extend this notation to write e(k,v,n,c) where n
%%	indicates the negation of k appearing as a literal in the clause identified by c.  Note that the
%%	negation of v is not recorded here.  This is because there will always be a similar connection leading
%%	from v back to k which will record the negation of v.
%%
%%	negation: The negation of the variable, k, appearing in the clause which creates this connection
%%		true | false
%%	clause_id: A Term() which uniquely identifies a clause in the formula
%%		Term()
%%	linked_literal: The literal also appearing in the clause with k
%%		#literal
%%
-record(connection,{negation,clause_id,linked_literal}).

%%
%%	Exported Functions
%%

%%	Creational functions
-export([def_2_formula/1]).
%%	Enquiry functions
-export([get_var_vals/1,get_variable/2,get_variables/1,get_literal_negation/2,get_clauses/1,get_literals/2,get_variable_value/2,eval_literal/2,get_formula_satisfiable/1,get_literal_counts/1]).
%%	Modifying functions
-export([assign_variable/2,assign_variables/2]).

%%
%% API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Creational Functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a #cnf_graph made from a formula_def where every variable is assigned indetermined.
%%		Formula_Def
%%			#formula_def
%%
def_2_formula({formula_def,Clause_Defs}) ->
    Clauses = lists:map(fun formula_util:clause_def_2_clause/1,Clause_Defs),
	#cnf_graph{connection_dict=make_c_dict(Clause_Defs),variable_dict=formula_util:make_v_dict(Clauses),satisfiable=indetermined}.

%%******************************%%
%%	Local Creational functions	%%
%%******************************%%

%%
%%	Returns a dictionary mapping variables to lists of #connection records to represent the cnf formula
%%	defined by the list of #clause_defs provided.
%%		Clause_Defs
%%			[#clause_def]
%%
make_c_dict(Clause_Defs) ->
    make_c_dict(Clause_Defs,0).

%%
%%	Returns a dictionary mapping variables to lists of #connection records to represent the cnf formula
%%	defined by the list of #clause_defs provided.  Takes an additional integer parameter, Clause_Id which
%%	is used to uniquely identify each clause.
%%
%%	Clause_Defs: A list of #clause_def records
%%		[#clause_def]
%%
make_c_dict([],_Unused_Id) -> dict:new();

make_c_dict([Clause_Def|Clause_Defs],Clause_Id) ->
	Literals = Clause_Def#clause_def.literal_defs,
	dict:merge(fun (_K,Connections1,Connections2) -> util:combine_without_duplicates(Connections1,Connections2) end,connect_variables(Literals,Clause_Id),make_c_dict(Clause_Defs,Clause_Id+1)).

%%
%%	Returns a dictionary mapping each of the variables found in the list Literals to a list of #connection
%%	records establishing the connection between every literal in this clause.
%%
%%	Literals: A list of #literal_defs making up a clause in the formula
%%		#literal_def
%%	Clause_Id: An integer uniqely identifying this clause in the formula
%%		Integer()
%%
connect_variables(Literal_Defss,Clause_Id) ->
    connect_variables(Literal_Defss,Literal_Defss,Clause_Id).

%%
%%	Returns a dictionary mapping each of the variables found in the list Literals to a list of #connection
%%	records establishing the connection between every literal in this clause.  This function takes a duplicate
%%	copy of the list of records which is not reduced with each recursive call.  This list is required to allow
%%	every variable in the clause to be connected to every other variable.
%%
%%	Literals: A list of #literal_defs making up a clause in the formula
%%		#literal_def
%%	All_Literals: A copy of Literals, which will not be recursively reduced
%%		#literal_def
%%	Clause_Id: An integer uniqely identifying this clause in the formula
%%		Integer()
%%
connect_variables([],_All_Literal_Defs,_Clause_Id) ->
	dict:new();

connect_variables([Literal_Def|Literal_Defs],All_Literal_Defs,Clause_Id) ->
	Dict = connect_variables(Literal_Defs,All_Literal_Defs,Clause_Id),
    dict:store(Literal_Def#literal_def.l_name,connect_variable(Literal_Def,All_Literal_Defs,Clause_Id),Dict).

%%
%%	Returns a list of #connecton records representing all of the connections from Literal
%%	to every other variable in this clause, i.e. contained inside All_Literals.
%%
%%	Literal: The literal whose variable will be connected to every other variable in this clause
%%		#literal_def
%%	All_Literals: A list of #literal_defs making up an entire clause in the formula
%%				  Is guaranteed to contain Literal
%%		[#literal_def]
%%	Clause_Id: An integer uniquely identifiying this clause in the formula
%%		Integer()
%%
connect_variable(Literal_Def,All_Literal_Defs,Clause_Id) ->
	Connecting_Literals = lists:subtract(All_Literal_Defs,[Literal_Def]),
    Connect_Fun = fun(Linked_Literal_Def) ->
                          Negation = Literal_Def#literal_def.negation,
                          Linked_Literal=#literal{negation=Linked_Literal_Def#literal_def.negation,l_name=Linked_Literal_Def#literal_def.l_name},
                          #connection{negation=Negation,clause_id=Clause_Id,linked_literal=Linked_Literal}
                  end,
    lists:map(Connect_Fun,Connecting_Literals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Enquiry Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a list of pairs of variables and ternary values [{Atom(),true|false|indetermined}]
%%		Formula
%%			#cnf_graph
%%
get_var_vals(Formula) ->
    Variable_Dict = Formula#cnf_graph.variable_dict,
	dict:to_list(Variable_Dict).

%%
%%	Returns a list of variables for the formula provided
%%		Formula
%%			#cnf_graph
%%
get_variables(Formula) ->
    Variable_Dict = Formula#cnf_graph.variable_dict,
	dict:fetch_keys(Variable_Dict).

%%
%%	Returns a list of clauses for the formula provided.
%%	NB: This implementation is not optimised for this function call.  NOT FAST!!!
%%
%%		Formula
%%			#cnf_graph
%%
get_clauses(Formula) ->
    Connection_Dict = Formula#cnf_graph.connection_dict,
    Map_Connection_By_Clause = fun(Connection,Clause_Dict) ->
                           				Linked_Literal = Connection#connection.linked_literal,
                            			Clause_Id = Connection#connection.clause_id,
                            			dict:append(Clause_Id,Linked_Literal,Clause_Dict)
                               end,
    Map_Connections_By_Clause = fun(Variable,Clause_Dict) ->
                            		Connections = dict:fetch(Variable,Connection_Dict),
                            		lists:foldl(Map_Connection_By_Clause,Clause_Dict,Connections)
                    			end,
    Clause_Dict = lists:foldl(Map_Connections_By_Clause,dict:new(),dict:fetch_keys(Connection_Dict)),
	Fold_To_Clauses = fun(Clause_Id,Clauses) ->
                              Literals = dict:fetch(Clause_Id,Clause_Dict),
                              Clause = #clause{literals=sets:to_list(sets:from_list(Literals))},
                              [Clause|Clauses]
                      end,
    lists:foldl(Fold_To_Clauses,[],dict:fetch_keys(Clause_Dict)).

%%
%%	Returns a list of literals for the clause provided
%%		Clause
%%			#clause
%%		Formula: The formula in which this clause appears
%%			#cnf_graph
%%
get_literals(Clause,_Formula) ->
	Clause#clause.literals.

%%
%%	Returns either true, false or indetermined depending on the value of the variable provided
%%	in the formula provided.
%%	
%%	Beware the potentially misleading name of this function.  This does not take into account the
%%	negation of a literal just the value of it's variable's assignment.
%%
%%	Will throw an exception if the variable is not found in the formula
%%		Variable.  The variable whose value we seek
%%			atom()
%%		Formula.  The formula in which we seek it
%%			#cnf_graph
%%
get_variable_value(Variable,Formula) ->
    Variable_Dict = Formula#cnf_graph.variable_dict,
	dict:fetch(Variable,Variable_Dict).

%%
%%	Returns either true, false or indetermined depending on the value of the variable
%%	used in the literal provided with respect to the formula provided.
%%		Literal.  The literal whose value we seek
%%			#literal
%%		Formula.  The formula in which we seek it
%%			#cnf_graph
%%
get_literal_value(Literal,Formula) ->
    Variable_Name = Literal#literal.l_name,
	get_variable_value(Variable_Name,Formula).

%%
%%	Returns the name, string(), of this literal.
%%	The name of a literal is the name of its variable
%%		Literal
%%			#literal
%%		Formula, the formula in which this literal appears, here included only for consistency
%%			#cnf_graph
%%
get_variable(Literal,_Formula) ->
	Literal#literal.l_name.

%%
%%	Returns the negation of this literal, which is either true or false
%%	In the propositional calc:
%%	for the variable p, negation=true means p and negation=false means ~p
%%		Literal
%%			#literal
%%		Formula, the formula in which this literal appears, here included only for consistency
%%			#cnf_graph
%%
get_literal_negation(Literal,_Formula) ->
	Literal#literal.negation.

%%
%% 	Evaluates the literal provided
%%	A literal evaluates to true iff its negation is the same as it's value
%%	A literal evaluates to indetermined iff its value is indetermined
%%	Otherwise a literal evaluates to false
%%		Literal.  The literal to evaluate
%%			#literal
%%		Formula.  The formula from which this literal comes
%%			#cnf_graph
%%
eval_literal(Literal,Formula) ->
	Negation = Literal#literal.negation,
	Value = get_literal_value(Literal,Formula),
	ternary_logic:not3(ternary_logic:xor3(Negation,Value)).

%%
%%	Returns true (satisfied) false (not satisfiable) or indetermined (not determined)
%%	This assumes that the Formula provided is consistent and has the following properties:
%%		1. All clauses containing literals evaluating to true (subsumed) are removed from their formula
%%		2. All literals evaluating to false are removed from their clause
%%		This means that:
%%			1. Any formula containing no clauses is satisfied, i.e. all clauses contain at least one true literal
%%			2. Any formula containing an empty (no literals) clause is not satisfiable, i.e. each literal for this clause is false
%%
get_formula_satisfiable(Formula) ->
    Formula#cnf_graph.satisfiable.

%%
%%	Returns a list of #literal_count for every variable in this formula
%%
get_literal_counts(Formula) ->
    Connection_Dict = Formula#cnf_graph.connection_dict,
    Variable_Dict = Formula#cnf_graph.variable_dict,
    Fold_Literal_Count = fun(Connection,Literal_Count) ->
                                 case Connection#connection.negation of
                                     true
                                     	->	erlang:setelement(#literal_count.positive_count,Literal_Count,Literal_Count#literal_count.positive_count+1);
                                     false
                                       	->	erlang:setelement(#literal_count.negative_count,Literal_Count,Literal_Count#literal_count.negative_count+1)
                                 end
                         end,
    Generate_Literal_Count = fun(Variable_Name) ->
                                     Connections = dict:fetch(Variable_Name,Connection_Dict),
                                     lists:foldl(Fold_Literal_Count,#literal_count{variable_name=Variable_Name,positive_count=0,negative_count=0},Connections)
                             end,
    util:filter_map(fun(V_Name)->dict:fetch(V_Name,Variable_Dict) =:= indetermined end,Generate_Literal_Count,dict:fetch_keys(Connection_Dict)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Modifying Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a formula identical to the one provided but with the variable assignment applied
%%	
%%	NB: This function has the additional effect of recursively setting all unit literals created
%%		by this assignment
%%	
%%	Variable_Assignment: The variable name and the value (true or false) that it will be assigned
%%		#variable_assignment
%%	Formula: The #cnf_graph formula in which the variable will be assigned
%%
assign_variable(Variable_Assignment,Formula) ->
	V_Name = Variable_Assignment#variable_assignment.l_name,
	Value = Variable_Assignment#variable_assignment.value,
	set_variable(V_Name,Value,Formula).

%%
%%	Returns a formula identical to the one provided but with each of the variable assignments applied
%%	
%%	NB: This function has the additional effect of recursively setting all unit literals created
%%		by this assignment
%%	
%%	Variable_Assignments: The variable name and the value (true or false) that it will be assigned
%%		#variable_assignment
%%	Formula: The #cnf_graph formula in which the variable will be assigned
%%
assign_variables([],Formula) -> Formula;

assign_variables(Variable_Assignments,Formula) ->
	Fun = fun(Variable_Assignment,Acc_Formula) ->
                		V_Name = Variable_Assignment#variable_assignment.l_name,
						Value = Variable_Assignment#variable_assignment.value,
    					set_variable(V_Name,Value,Acc_Formula)
          end,
    Pred = fun(Acc)-> case Acc#cnf_graph.satisfiable of
                          indetermined
                            	->	true;
                          _True_Or_False
                            	->	false
                      end
           end,
    util:foldl_abandon(Fun,Formula,Variable_Assignments,Pred).

%%******************************%%
%%	Local Modifying functions	%%
%%******************************%%

%%
%%	Returns a new #cnf_graph where the variable indicated by V_Name has been assigned Value.
%%	In this representation of a cnf formula assigning a variable has two distinct steps.
%%
%%	NB:	Below we refer to #connection records as literals interchageably.  This is because each
%%		#connection record represents the connection between a literal and another literal in a 
%%		particular clause in the formula.  See the #cnf_graph definition above.
%%
%%	1: Set cnf_graph.variable_dict so that V_Name -> Value
%%	2: Update the graph representation of the formula, which is done in two steps
%%		i) 	Remove every #connection mapping from V_Name where Value would make that literal true.
%%		ii)	Remove every edge in the graph with the same clause ID as any edge removed in step 1.
%%
set_variable(V_Name,Value,Formula) ->
	Variable_Dict = Formula#cnf_graph.variable_dict,
    Connection_Dict = Formula#cnf_graph.connection_dict,
    New_V_Dict = dict:store(V_Name,Value,Variable_Dict),
    {New_C_Dict,{Satisfiable,Unit_Literal}} = cut_connections(V_Name,Value,Connection_Dict,New_V_Dict),
    case Unit_Literal of
        ?NULL_LITERAL
        	->	#cnf_graph{connection_dict=New_C_Dict,variable_dict=New_V_Dict,satisfiable=Satisfiable};
        Unit_Literal
        	->	New_Formula = #cnf_graph{connection_dict=New_C_Dict,variable_dict=New_V_Dict,satisfiable=Satisfiable},
                Variable_Assignment = #variable_assignment{l_name=Unit_Literal#literal.l_name,value=Unit_Literal#literal.negation},
    			assign_variable(Variable_Assignment,New_Formula)
    end.

%%
%%	Returns a Dict() which is the same is Connection_Dict except:
%%		1:	Where the list of #connections mapped from V_Name does not contain any #connection
%%			records where Value would make that literal true.
%%		2:	No #connection will exist in the returned Dict() whose clause_id is the same as any
%%			#connection removed in order to make the first property true.
%%
%%	V_Name: The name of the variable whose value is being assigned
%%		Atom()
%%	Value: The Value being assigned to V_Name
%%		true | false
%%	Connection_Dict: A dictionary mapping variable names to lists of #connection records
%%		Dict(): Atom() -> [#connection]
%%
cut_connections(V_Name,Value,Connection_Dict,Variable_Dict) ->
	Connections = dict:fetch(V_Name,Connection_Dict),
    {True_Connections,False_Connections} = seperate_true_connections(Value,Connections),
    New_C_Dict = remove_by_clauses(True_Connections,Connection_Dict),
    case False_Connections of
        []
        	->	Final_C_Dict = dict:erase(V_Name,New_C_Dict),
                {Final_C_Dict,{ternary_logic:or3(dict:size(Final_C_Dict)=:=0,indetermined),?NULL_LITERAL}};
    	_List
			->	Final_C_Dict = dict:store(V_Name,False_Connections,New_C_Dict),
                {Final_C_Dict,check_effected_clauses(False_Connections,Variable_Dict)}
    end.

%%
%%	Returns two lists, one containing a list of #connections where Value would make that literal true
%%	and the other where Value would make it false.
%%
%%	Value: The value being assigned to the literals described by Connections
%%		true | false
%%	Connections: A list of #connections describing a set of literals (for a single variable) appearing in the formula
%%		[#connection]
%%
seperate_true_connections(Value,Connections) ->
	Sep_Fun = fun(Connection,{True_List,False_List}) ->
                      Negation = Connection#connection.negation,
                      case Value =:= Negation  of
                      		true
                         		->	{[Connection|True_List],False_List};
                      		false
              					->	{True_List,[Connection|False_List]}
                      end
               end,
	lists:foldl(Sep_Fun,{[],[]},Connections).

%%
%%	Returns a Dict() which is the same is Connection_Dict except:
%%		1:	No #connection will exist in the returned Dict() whose clause_id is the same as any
%%			#connection in True_Connections.
%%
%%	True_Connections: A list of #connections whose literals are now true
%%		[#connection]
%%	Connection_Dict: A dictionary mapping variable names to lists of #connection records
%%		Dict(): Atom() -> [#connection]
%%
remove_by_clauses(True_Connections,Connection_Dict) ->
	Remove_Fun = fun(Connection,Acc_Dict) ->
                         Linked_Literal = Connection#connection.linked_literal,
                         Linked_Variable = Linked_Literal#literal.l_name,
                         Clause_Id = Connection#connection.clause_id,
                         Variable_Connections = dict:fetch(Linked_Variable,Acc_Dict),
                         Filtered_Connections = lists:filter(fun(V_Con)-> V_Con#connection.clause_id /= Clause_Id end,Variable_Connections),
                         case Filtered_Connections of
                             []
                             	->	dict:erase(Linked_Variable,Acc_Dict);
                             _List
                             	->	dict:store(Linked_Variable,Filtered_Connections,Acc_Dict)
                         end
                 end,
    lists:foldl(Remove_Fun,Connection_Dict,True_Connections).

%%
%%	Returns {Satisfiable,Unit_Literal}.  If any clause, identified in Connections, contains
%%	only literals that are falsified by the values assigned by Variable_Dict then Satisfiable 
%%	is false.  If every clause contains at least one literal which remains indetermined then 
%%	Satisfiable is indetermined.  Unit_Literal is either null_literal, indicating that no unit
%%	literals were found, or is a #literal which is unit in the formula being processed.
%%
%%	The reason we only return a single unit literal is because unit literals are resolved recursively
%%	which means remaining unit literals will be picked up by subsequent calls to assign_variable until
%%	no unit literals remain.
%%
check_effected_clauses(Connections,Variable_Dict) ->
    Group_By_Clause = fun(Connection,Acc_Dict) ->
                              Clause_Id = Connection#connection.clause_id,
                              dict:append(Clause_Id,Connection,Acc_Dict)
                      end,
    Clause_Dict = lists:foldl(Group_By_Clause,dict:new(),Connections),
    Acc_Indetermined_Literals = fun(Connection,Indetermined_Literals) ->
                                        Linked_Literal = Connection#connection.linked_literal,
                                 		Linked_Variable = Linked_Literal#literal.l_name,
                               	 		case dict:fetch(Linked_Variable,Variable_Dict) of
                              					indetermined
                                   		  			->	[Linked_Literal|Indetermined_Literals];
                                 				_True_Or_False
                         							->	Indetermined_Literals
								 		end
                         		end,
    Is_Sat_Acc_Unit_Literals = fun(Clause_Id,Acc = {Satisfiable,Unit_Literal}) ->
                                     case lists:foldl(Acc_Indetermined_Literals,[],dict:fetch(Clause_Id,Clause_Dict)) of
                                     	[] %% Unsatisfiable clause
                                     		->	{false,Unit_Literal};
                                     	[Unit_Literal|[]] %% Unit clause
                       						->	{Satisfiable,Unit_Literal};
                                        _Normal_Clause
                                          	->	Acc
                                     end
                                end,
	lists:foldl(Is_Sat_Acc_Unit_Literals,{indetermined,?NULL_LITERAL},dict:fetch_keys(Clause_Dict)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%	Test Functions	%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

contains_variable(V_Name,Connection_Dict) ->
    Find_Variable_In_Connection = fun(Connection,Found) ->
                                          Linked_Literal = Connection#connection.linked_literal,
                                          Found or (Linked_Literal#literal.l_name =:= V_Name)
                                  end,
    Find_Variable_In_Connections = fun(Dict_Key,Found) ->
                                           Connections = dict:fetch(Dict_Key,Connection_Dict),
                                           Found or (Dict_Key =:= V_Name) or lists:foldl(Find_Variable_In_Connection,false,Connections)
                                   end,
    lists:foldl(Find_Variable_In_Connections,false,dict:fetch_keys(Connection_Dict)).

is_consistent(Connection_Dict) ->
    Collect_Connections = fun(Variable_Name) ->
                                  Find_Referencing_Connections = fun(Key,Acc_Connections) ->
                                                                         Check_Connections = dict:fetch(Key,Connection_Dict),
                                                                         Referencing_Connections = [Connection#connection.linked_literal||Connection<-Check_Connections,(Connection#connection.linked_literal)#literal.l_name=:=Variable_Name],
                                                                         case Referencing_Connections of
                                                                             []
                                                                               	->	Acc_Connections;
                                                                         	 _List
                                                                 				->	[Key|Acc_Connections]
                                                                         end
                                                                 end,
                                  {Variable_Name,lists:foldl(Find_Referencing_Connections,[],dict:fetch_keys(Connection_Dict))}
                          end,
    Variables_And_Connections = lists:map(Collect_Connections,dict:fetch_keys(Connection_Dict)),
	Check_Variable = fun({Variable_Name,Connected_Literals}) ->
                             Direct_Connected_Literals = [(Connection#connection.linked_literal)#literal.l_name||Connection<-dict:fetch(Variable_Name,Connection_Dict)],
                             One = lists:all(fun(Literal)->lists:member(Literal,Direct_Connected_Literals) end,Connected_Literals),
                             Two = lists:all(fun(Literal)->lists:member(Literal,Connected_Literals) end,Direct_Connected_Literals),
                             One and Two
                     end,
    lists:map(Check_Variable,Variables_And_Connections).
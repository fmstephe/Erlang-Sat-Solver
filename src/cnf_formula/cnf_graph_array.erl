%% Author: Francis Stephens
%% Created: 25 Feb 2008
%% Description: TODO: Add desciption to cnf_formula
-module(cnf_graph_array).

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
%%	variable_connections: An array of #connection lists, each index represents the 
%%		dict(): Atom() -> [#connection]
%%	variable_values: The dictionary mapping variables to their assigned values
%%		dict(): Atom() -> (true | false | indetermined)
%%	variable_names: 
%%	satisfiable: A flag indicating whether the formula, with given variable assignments, has been determined
%%				 to be satisfiable
%%		true | false | indetermined
%%
-record(cnf_graph,{variable_connections,variable_values,variable_names,satisfiable}).

%%
%%	TODO these comments are wrong!
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
-record(connection,{negation,clause_id,linked_variable_index,linked_negation}).

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
def_2_formula(Formula_Def) ->
    Clause_Defs = Formula_Def#formula_def.clause_defs,
    {Variable_Names,Variable_Values,Name_Index_Dict} = make_variable_names(Clause_Defs),
    #cnf_graph{variable_connections=make_connections_array(Clause_Defs,Name_Index_Dict),variable_values=Variable_Values,variable_names=Variable_Names,satisfiable=indetermined}.

%%******************************%%
%%	Local Creational functions	%%
%%******************************%%


%%	TODO these comments are incorrect
make_variable_names(Clause_Defs) ->
    Make_Name_Set = fun(Clause_Def,Name_Set) ->
                            Literals = Clause_Def#clause_def.literal_defs,
                            Raw_Names = [Literal#literal_def.l_name||Literal<-Literals],
                            New_Name_Set = sets:from_list(Raw_Names),
                            sets:union(Name_Set,New_Name_Set)
                    end,
    All_Names = lists:foldl(Make_Name_Set,sets:new(),Clause_Defs),
    Number_Of_Names = sets:size(All_Names),
    All_Indexes = lists:seq(0,Number_Of_Names-1),
    Indexed_Names = lists:zip(All_Indexes,sets:to_list(All_Names)),
    Add_Name_To_Array = fun({Index,Name},Name_Array) ->
                                array:set(Index,Name,Name_Array)
                        end,
    Variable_Names = lists:foldl(Add_Name_To_Array,array:new(sets:size(All_Names)),Indexed_Names),
    Map_Name_To_Index = fun(Index,Name_Index_Dict) ->
                                Name = array:get(Index,Variable_Names),
                                dict:store(Name,Index,Name_Index_Dict)
                        end,
    Name_Index_Dict = lists:foldl(Map_Name_To_Index,dict:new(),All_Indexes),
    Variable_Values =  array:new(Number_Of_Names,{default,indetermined}),
    {Variable_Names,Variable_Values,Name_Index_Dict}.
    
%%
%%	TODO these comments are incorrect
%%	Returns a dictionary mapping variables to lists of #connection records to represent the cnf formula
%%	defined by the list of #clause_defs provided.
%%		Clause_Defs
%%			[#clause_def]
%%
make_connections_array(Clause_Defs,Name_Index_Dict) ->
    make_connections_array(Clause_Defs,Name_Index_Dict,0).

%%
%%	TODO these comments are incorrect
%%	Returns a dictionary mapping variables to lists of #connection records to represent the cnf formula
%%	defined by the list of #clause_defs provided.  Takes an additional integer parameter, Clause_Id which
%%	is used to uniquely identify each clause.
%%
%%	Clause_Defs: A list of #clause_def records
%%		[#clause_def]
%%
make_connections_array([],Name_Index_Dict,_Unused_Id) -> array:new(dict:size(Name_Index_Dict),{default,[]});

make_connections_array([Clause_Def|Clause_Defs],Name_Index_Dict,Clause_Id) ->
	Literal_Defs = Clause_Def#clause_def.literal_defs,
    Make_Connections = fun(Literal_Def) ->
                              Variable_Name = Literal_Def#literal_def.l_name,
                              Variable_Index = dict:fetch(Variable_Name,Name_Index_Dict),
                              Negation = Literal_Def#literal_def.negation,
                              Connected_Literals = lists:delete(Literal_Def,Literal_Defs),
                              Make_Connection = fun(Linked_Literal_Def) ->
                                                        Linked_Variable_Name = Linked_Literal_Def#literal_def.l_name,
                                                        Linked_Negation = Linked_Literal_Def#literal_def.negation,
                                                        Linked_Variable_Index = dict:fetch(Linked_Variable_Name,Name_Index_Dict),
                                                        #connection{negation=Negation,clause_id=Clause_Id,linked_variable_index=Linked_Variable_Index,linked_negation=Linked_Negation}
                                                end,
                              {Variable_Index,lists:map(Make_Connection,Connected_Literals)}
                      end,
    Connection_List = lists:map(Make_Connections,Literal_Defs),
    Fold_Connections = fun({Variable_Index,Connections},Connections_Array) ->
                               Extant_Connections = array:get(Variable_Index,Connections_Array),
                               array:set(Variable_Index,Connections++Extant_Connections,Connections_Array)
                       end,
    lists:foldl(Fold_Connections,make_connections_array(Clause_Defs,Name_Index_Dict,Clause_Id+1),Connection_List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Enquiry Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a list of pairs of variables and ternary values [{Atom(),true|false|indetermined}]
%%		Formula
%%			#cnf_graph
%%
get_var_vals(Formula) ->
    Variable_Values = Formula#cnf_graph.variable_values,
    Variable_Names = Formula#cnf_graph.variable_names,
    Raw_Variable_List = array:to_orddict(Variable_Values),
    Add_Variable_Name = fun({Index,Value}) ->
                                {array:get(Index,Variable_Names),Value}
                        end,
    lists:map(Add_Variable_Name,Raw_Variable_List).

%%
%%	Returns a list of variables for the formula provided
%%		Formula
%%			#cnf_graph
%%
get_variables(Formula) ->
    Variable_Connections = Formula#cnf_graph.variable_connections,
	util:get_indices(Variable_Connections).

%%
%%	Returns a list of clauses for the formula provided.
%%	NB: This implementation is not optimised for this function call.  NOT FAST!!!
%%
%%		Formula
%%			#cnf_graph
%%
get_clauses(Formula) ->
    Variable_Names = Formula#cnf_graph.variable_names,
    Variable_Connections = Formula#cnf_graph.variable_connections,
    Map_Connection_By_Clause = fun(Connection,Clause_Dict) ->
                           				Linked_Variable_Index = Connection#connection.linked_variable_index,
                                        Linked_Negation = Connection#connection.linked_negation,
                                        Linked_Literal = #literal{l_name=Linked_Variable_Index,negation=Linked_Negation},
                            			Clause_Id = Connection#connection.clause_id,
                            			dict:append(Clause_Id,Linked_Literal,Clause_Dict)
                               end,
    Map_Connections_By_Clause = fun(Variable_Index,Clause_Dict) ->
                            		Connections = array:get(Variable_Index,Variable_Connections),
                            		lists:foldl(Map_Connection_By_Clause,Clause_Dict,Connections)
                    			end,
    Clause_Dict = lists:foldl(Map_Connections_By_Clause,dict:new(),util:get_indices(Variable_Names)),
	Fold_To_Clauses = fun(Clause_Id,Clauses) ->
                              Literals = dict:fetch(Clause_Id,Clause_Dict),
                              Clause = #clause{literals=sets:to_list(sets:from_list(Literals))},
                              [Clause|Clauses]
                      end,
    Return = lists:foldl(Fold_To_Clauses,[],dict:fetch_keys(Clause_Dict)),
    Return.

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
get_variable_value(Variable_Index,Formula) ->
    Variable_Values = Formula#cnf_graph.variable_values,
	array:get(Variable_Index,Variable_Values).

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
    Variable_Index = Literal#literal.l_name,
    Literal_Negation = Literal#literal.negation,
    Variable_Values = Formula#cnf_graph.variable_values,
    Variable_Value = array:get(Variable_Index,Variable_Values),
    ternary_logic:equal3(Variable_Value,Literal_Negation).

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
    Variable_Connections = Formula#cnf_graph.variable_connections,
    Variable_Values = Formula#cnf_graph.variable_values,
    Fold_Literal_Count = fun(Connection,Literal_Count) ->
                                 case Connection#connection.negation of
                                     true
                                     	->	erlang:setelement(#literal_count.positive_count,Literal_Count,Literal_Count#literal_count.positive_count+1);
                                     false
                                       	->	erlang:setelement(#literal_count.negative_count,Literal_Count,Literal_Count#literal_count.negative_count+1)
                                 end
                         end,
    Generate_Literal_Count = fun(Variable_Index) ->
                                     Connections = array:get(Variable_Index,Variable_Connections),
                                     lists:foldl(Fold_Literal_Count,#literal_count{variable_name=Variable_Index,positive_count=0,negative_count=0},Connections)
                             end,
    util:filter_map(fun(V_Index)->array:get(V_Index,Variable_Values) =:= indetermined end,Generate_Literal_Count,util:get_indices(Variable_Connections)).

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
	V_Index = Variable_Assignment#variable_assignment.l_name,
	Value = Variable_Assignment#variable_assignment.value,
	set_variable(V_Index,Value,Formula).

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
                		V_Index = Variable_Assignment#variable_assignment.l_name,
						Value = Variable_Assignment#variable_assignment.value,
    					set_variable(V_Index,Value,Acc_Formula)
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
%%	TODO these comments are wrong
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
set_variable(V_Index,Value,Formula) ->
    Variable_Connections = Formula#cnf_graph.variable_connections,
    Variable_Values = Formula#cnf_graph.variable_values,
    Variable_Names = Formula#cnf_graph.variable_names,
    New_V_Values = array:set(V_Index,Value,Variable_Values),
    {New_V_Connections,{Satisfiable,Unit_Literal},Removable_Variables} = cut_connections(V_Index,Value,Variable_Connections,New_V_Values),
    case Unit_Literal of
        ?NULL_LITERAL
        	->	dont_clean_formula(New_V_Connections,New_V_Values,Variable_Names,Satisfiable,Removable_Variables);
        Unit_Literal
        	->	New_Formula = dont_clean_formula(New_V_Connections,New_V_Values,Variable_Names,Satisfiable,Removable_Variables),
                Variable_Assignment = #variable_assignment{l_name=Unit_Literal#literal.l_name,value=Unit_Literal#literal.negation},
    			assign_variable(Variable_Assignment,New_Formula)
    end.

dont_clean_formula(Variable_Connections,Variable_Values,Variable_Names,Satisfiable,_Removable_Variables) ->
    #cnf_graph{variable_connections=Variable_Connections,
               variable_values=Variable_Values,
               variable_names=Variable_Names,
			   satisfiable=Satisfiable}.

%%
%%	Removes each variable in Removable_Variables from each of the arrays that make up a formula.
%%	
%%	Variable_Connections: Array mapping each variable (as an index) to a list of #connections
%%		Array()
%%	Variable_Values: Array mapping each variable (as an index) to a list of values (true | false | indetermined)
%%		Array()
%%	Variable_Names: Array mapping each variable (as an index) to a list of variable names (Atom())
%%		Array()
%%	Satisfiable: Indicates whether this formula is satisfiable
%%		true | false | indetermined
%%	Removable_Variables: A list of variable indexes which can be removed (because they no longer appear in unsatisfied clauses)
%%
make_clean_formula(Variable_Connections,Variable_Values,Variable_Names,Satisfiable,Removable_Variables) ->
    Sorted_R_Variables = lists:sort(fun(A,B)-> A > B end,Removable_Variables),
    New_Variable_Connections = update_connection_indices(Sorted_R_Variables,Variable_Connections),
	New_Variable_Values = util:delete_elements_unsafe(Sorted_R_Variables,Variable_Values),
    New_Variable_Names = util:delete_elements_unsafe(Sorted_R_Variables,Variable_Names),
    Is_Empty = array:size(New_Variable_Connections)=:=0,
    New_Satisfiable = ternary_logic:or3(Is_Empty,Satisfiable),
    Return = #cnf_graph{variable_connections=New_Variable_Connections,
               variable_values=New_Variable_Values,
               variable_names=New_Variable_Names,
			   satisfiable=New_Satisfiable},
    Return.

%%
%%
%%
update_connection_indices([],Variable_Connections) ->
	Variable_Connections;

update_connection_indices(Removable_Variables,Variable_Connections) ->
    Cleaned_Variable_Connections = util:delete_elements_unsafe(Removable_Variables,Variable_Connections),
    Reversed_Removable_Variables = lists:reverse(Removable_Variables),
    Update_Connection = fun(Connection) ->
                                Variable_Index = Connection#connection.linked_variable_index,
                                case Variable_Index > hd(Reversed_Removable_Variables) of
                                    true
                                    	->	Offset = update_index(Variable_Index,Reversed_Removable_Variables),
                                			erlang:setelement(#connection.linked_variable_index,Connection,Variable_Index-Offset);
                                    false
                                    	->	Connection
                                end
                        end,
    Update_Connection_Array = fun(Variable_Index,Acc_Array) ->
                                      Connections = array:get(Variable_Index,Acc_Array),
                                      New_Connections = lists:map(Update_Connection,Connections),
                                      array:set(Variable_Index,New_Connections,Acc_Array)
                              end,
    Return = lists:foldl(Update_Connection_Array,Cleaned_Variable_Connections,util:get_indices(Cleaned_Variable_Connections)),
    Return.

update_index(Variable_Index,Removable_Variables) ->
    update_index(Variable_Index,Removable_Variables,0).

update_index(_Variable_Index,[],Offset) -> Offset;

update_index(Variable_Index,[Removable_Variable|Removable_Variables],Offset) ->
    case Variable_Index > Removable_Variable of
        true
        	->	update_index(Variable_Index,Removable_Variables,Offset+1);
        false
        	->	Offset
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
cut_connections(V_Index,Value,Variable_Connections,Variable_Values) ->
    Connections = array:get(V_Index,Variable_Connections),
    {True_Connections,False_Connections} = seperate_true_connections(Value,Connections),
    {Int_V_Connections,Removable_Variables} = remove_by_clauses(True_Connections,Variable_Connections),
    Final_Removable_Variables = set_v_index_removable(V_Index,False_Connections,Removable_Variables),
	Final_V_Connections = array:set(V_Index,False_Connections,Int_V_Connections),
    {Final_V_Connections,check_effected_clauses(False_Connections,Variable_Values),Final_Removable_Variables}.

%%
%%	Returns the Removable_Connections list with V_Index added if False_Connections is empty.
%%
%%	V_Index: 
%%		Integer()
%%	False_Connections: List of connections (literals) which were made false by the current variable assignment
%%		[#connection]
%%	Removable_Connections: List of variable indexes which will be removed from this formula
%%		[Integer()]
%%
set_v_index_removable(V_Index,[],Removable_Connections) ->
	[V_Index|Removable_Connections];

set_v_index_removable(_V_Index,_False_Connections,Removable_Connections) ->
	Removable_Connections.

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
remove_by_clauses(True_Connections,Variable_Connections) ->
	Remove_Fun = fun(Connection,{Acc_Array,Empty_Variables}) ->
                         Linked_Variable_Index = Connection#connection.linked_variable_index,
                         Clause_Id = Connection#connection.clause_id,
                         Connections = array:get(Linked_Variable_Index,Acc_Array),
                         Filtered_Connections = lists:filter(fun(V_Con)-> V_Con#connection.clause_id /= Clause_Id end,Connections),
                         case Filtered_Connections of
                             []
                             	->	{Acc_Array,[Linked_Variable_Index|Empty_Variables]};
                             _List
                             	->	{array:set(Linked_Variable_Index,Filtered_Connections,Acc_Array),Empty_Variables}
                         end
                 end,
    lists:foldl(Remove_Fun,{Variable_Connections,[]},True_Connections).

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
check_effected_clauses(Connections,Variable_Values) ->
    Group_By_Clause = fun(Connection,Acc_Dict) ->
                              Clause_Id = Connection#connection.clause_id,
                              dict:append(Clause_Id,Connection,Acc_Dict)
                      end,
    Clause_Dict = lists:foldl(Group_By_Clause,dict:new(),Connections),
    Acc_Indetermined_Literals = fun(Connection,Indetermined_Literals) ->
                                        Linked_Variable_Index = Connection#connection.linked_variable_index,
                                        Linked_Negation = Connection#connection.linked_negation,
                                        Linked_Literal = #literal{l_name=Linked_Variable_Index,negation=Linked_Negation},
                               	 		case array:get(Linked_Variable_Index,Variable_Values) of
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

contains_variable(V_Index,Variable_Connections) ->
    Find_Variable_In_Connection = fun(Connection,Found) ->
                                          Linked_Variable_Index = Connection#connection.linked_variable_index,
                                          Found or (Linked_Variable_Index =:= V_Index)
                                  end,
    Find_Variable_In_Connections = fun(Index,Found) ->
                                           Connections = array:get(Index,Variable_Connections),
                                           Found or (Index =:= V_Index and (length(Connections) /= 0)) or lists:foldl(Find_Variable_In_Connection,false,Connections)
                                   end,
    lists:foldl(Find_Variable_In_Connections,false,dict:fetch_keys(Variable_Connections)).

is_consistent(Variable_Connections) ->
    Collect_Connections = fun(Variable_Index) ->
                                  Find_Referencing_Connections = fun(Index,Acc_Connections) ->
                                                                         Check_Connections = array:get(Index,Variable_Connections),
                                                                         Referencing_Connections = [Connection#connection.linked_variable_index||Connection<-Check_Connections,Connection#connection.linked_variable_index=:=Variable_Index],
                                                                         case Referencing_Connections of
                                                                             []
                                                                               	->	Acc_Connections;
                                                                         	 _List
                                                                 				->	[Index|Acc_Connections]
                                                                         end
                                                                 end,
                                  {Variable_Index,lists:foldl(Find_Referencing_Connections,[],util:get_indices(Variable_Connections))}
                          end,
    Variables_And_Connections = lists:map(Collect_Connections,util:get_indices(Variable_Connections)),
	Check_Variable = fun({Variable_Index,Connected_Indexes}) ->
                             Direct_Connected_Indexes = [Connection#connection.linked_variable_index||Connection<-array:get(Variable_Index,Variable_Connections)],
                             One = lists:all(fun(Index)->lists:member(Index,Direct_Connected_Indexes) end,Connected_Indexes),
                             Two = lists:all(fun(Index)->lists:member(Index,Connected_Indexes) end,Direct_Connected_Indexes),
                             One and Two
                     end,
	lists:map(Check_Variable,Variables_And_Connections).

print_formula(Formula) ->
    Variable_Connections = Formula#cnf_graph.variable_connections,
    Variable_Values = Formula#cnf_graph.variable_values,
    Variable_Names = Formula#cnf_graph.variable_names,
    Satisfiable = Formula#cnf_graph.satisfiable,
    io:format("~n~n*******************************************************************"),
    io:format("~n~nVariable Connections = ~p~n~nVariable Values = ~p~n~nVariable Names = ~p~n~nSatisfiable = ~p~n~n",[
        array:to_orddict(Variable_Connections),
        array:to_orddict(Variable_Values),
        array:to_orddict(Variable_Names),
        Satisfiable]),
    io:format("~n~n*******************************************************************").



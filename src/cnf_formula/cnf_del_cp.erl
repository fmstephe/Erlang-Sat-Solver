%% Author: Francis Stephens
%% Created: 25 Feb 2008
%% Description: TODO: Add desciption to cnf_formula
-module(cnf_del_cp).

%%
%%	Include files
%%

-include("sat_records.hrl").
-include("cnf_formula_records.hrl").
-include("logging_records.hrl").

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
%%	Macros
%%

%%
%%	The unsatisfiable clause
%%

-define(UNSAT_CLAUSE,{clause,[]}).

%%
%% API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Creational Functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a formula made from a formula_def where every variable is assigned indetermined.
%%		Formula_Def
%%			#formula_def
%%
def_2_formula({formula_def,Clause_Defs}) ->
    Clauses = lists:map(fun formula_util:clause_def_2_clause/1,Clause_Defs),
	Created_Formula = #formula{variable_dict=formula_util:make_v_dict(Clauses),clauses=Clauses},
    %% io:format("~n~nCreated Formula = ~p~n~n",[Created_Formula]),
    Created_Formula.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Enquiry Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a list of pairs of variables and ternary values [{Atom(),true|false|indetermined}]
%%		Formula
%%			#formula
%%
get_var_vals({formula,Variable_Dict,_Clauses}) ->
	dict:to_list(Variable_Dict).

%%
%%	Returns a list of variables for the formula provided
%%		Formula
%%			#formula
%%
get_variables({formula,Variable_Dict,_Clauses}) ->
	dict:fetch_keys(Variable_Dict).

%%
%%	Returns a list of clauses for the formula provided
%%		Formula
%%			#formula
%%
get_clauses({formula,_Variable_Dict,Clauses}) -> Clauses.

%%
%%	Returns a list of literals for the clause provided
%%		Clause
%%			#clause
%%		Formula: The formula in which this clause appears
%%			#formula
%%
get_literals({clause,Literals},_Formula) ->
	Literals.

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
%%			#formula
%%
get_variable_value(Variable,{formula,Variable_Dict,_Clauses}) ->
	dict:fetch(Variable,Variable_Dict).

%%
%%	Returns either true, false or indetermined depending on the value of the variable
%%	used in the literal provided with respect to the formula provided.
%%		Literal.  The literal whose value we seek
%%			#literal
%%		Formula.  The formula in which we seek it
%%			#formula
%%
get_literal_value({literal,_Negation,L_Name},Formula) ->
	get_variable_value(L_Name,Formula).

%%
%%	Returns the name, string(), of this literal.
%%	The name of a literal is the name of its variable
%%		Literal
%%			#literal
%%		Formula, the formula in which this literal appears, here included only for consistency
%%			#formula
%%
get_variable({literal,_Negation,L_Name},_Formula) ->
	L_Name.

%%
%%	Returns the negation of this literal, which is either true or false
%%	In the propositional calc:
%%	for the variable p, negation=true means p and negation=false means ~p
%%		Literal
%%			#literal
%%		Formula, the formula in which this literal appears, here included only for consistency
%%			#formula
%%
get_literal_negation({literal,Negation,_L_Name},_Formula) ->
	Negation.

%%
%% 	Evaluates the literal provided
%%	A literal evaluates to true iff its negation is the same as it's value
%%	A literal evaluates to indetermined iff its value is indetermined
%%	Otherwise a literal evaluates to false
%%		Literal.  The literal to evaluate
%%			#literal
%%		Formula.  The formula from which this literal comes
%%			#formula
%%	
%%
eval_literal(Literal,Formula) ->
	Negation = get_literal_negation(Literal,Formula),
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
get_formula_satisfiable({formula,_Variable_Dict,[]}) -> true;

get_formula_satisfiable({formula,_Variable_Dict,Clauses}) ->
	case lists:member(?UNSAT_CLAUSE,Clauses) of
			true
				->  false;
			false
				-> indetermined
	end.

%%
%%	
%%
get_literal_counts(Formula) ->
	Clauses = cnf_del_fast:get_clauses(Formula),
	Literal_Count = count_literals(Clauses,Formula,dict:new()),
	{_Variables,Literal_Counts} = lists:unzip(dict:to_list(Literal_Count)),
    Literal_Counts.

%%
%%	
%%
count_literals([],_Formula,Counting_Dict) ->
    Counting_Dict;

count_literals([Clause|Clauses],Formula,Counting_Dict) ->
	Literals = cnf_del_fast:get_literals(Clause,Formula),
	Literal_Counter = fun(Literal,Literal_Count_Dict) ->
						Variable_Name = Literal#literal.l_name,
                        Negation = Literal#literal.negation,
                        Value = get_variable_value(Variable_Name,Formula),
                        update_literal_count(Variable_Name,Value,Negation,Literal_Count_Dict)
					  end,
	New_Dict = lists:foldl(Literal_Counter,Counting_Dict,Literals),
	count_literals(Clauses,Formula,New_Dict).

%%
%%
%%
update_literal_count(Variable_Name,indetermined,true,Literal_Count_Dict) ->
	case dict:find(Variable_Name,Literal_Count_Dict) of
        {ok,Literal_Count}
        	->	New_Literal_Count = erlang:setelement(#literal_count.positive_count,Literal_Count,Literal_Count#literal_count.positive_count+1),
				dict:store(Variable_Name,New_Literal_Count,Literal_Count_Dict);
    	error
			->	New_Literal_Count = #literal_count{variable_name=Variable_Name,positive_count=1,negative_count=0},
                dict:store(Variable_Name,New_Literal_Count,Literal_Count_Dict)
    end;

update_literal_count(Variable_Name,indetermined,false,Literal_Count_Dict) ->
	case dict:find(Variable_Name,Literal_Count_Dict) of
        {ok,Literal_Count}
        	->	New_Literal_Count = erlang:setelement(#literal_count.negative_count,Literal_Count,Literal_Count#literal_count.negative_count+1),
				dict:store(Variable_Name,New_Literal_Count,Literal_Count_Dict);
    	error
			->	New_Literal_Count = #literal_count{variable_name=Variable_Name,positive_count=0,negative_count=1},
                dict:store(Variable_Name,New_Literal_Count,Literal_Count_Dict)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Modifying Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a formula identical to the one provided but with the variable assignment applied
%%		Formula
%%			#formula
%%		Variable_Assignment
%%			#variable_assignment
%%
assign_variable(Variable_Assignment,Formula) ->
	Inc_Formula = set_variable(Variable_Assignment,Formula),
	{New_Formula,Unit_Literals} = check_clauses(Inc_Formula),
    case Unit_Literals of
        []
        	->	New_Formula;
        Unit_Literals
        	->	%% io:format("~n~nFormula = ~p~n~nUnit Literals = ~p~n~nNew_Formula = ~p~n~n",[Formula,Unit_Literals,New_Formula]),
                assign_variables(Unit_Literals,New_Formula)
    end.

%%
%%	Returns a formula identical to the one provided but with each of the variable assignments applied
%%		Formula
%%			#formula
%%		Variable_Assignments
%%			[#variable_assignment]
%%
assign_variables([],Formula) -> Formula;

assign_variables(Assignments,Formula) ->
    Inc_Formula = lists:foldl(fun set_variable/2,Formula,Assignments),
	{New_Formula,Unit_Literals} = check_clauses(Inc_Formula),
    case Unit_Literals of
        []
        	->	New_Formula;
        Unit_Literals
        	->	%% io:format("~n~nFormula = ~p~n~nUnit Literals = ~p~n~nNew_Formula = ~p~n~n",[Formula,Unit_Literals,New_Formula]),
                assign_variables(Unit_Literals,New_Formula)
    end.

%%******************************%%
%%	Local Modifying functions	%%
%%******************************%%

%%
%%
%%
set_variable({variable_assignment,V_Name,Value},{formula,Variable_Dict,Clauses}) ->
    %% io:format("~n~nVariable Assignment = ~p~n~n",[{variable_assignment,V_Name,Value}]),
    New_V_Dict = dict:store(V_Name,Value,Variable_Dict),
	#formula{variable_dict=New_V_Dict,clauses=Clauses};

%%
%%	This version is called for unit literals processing
%%
set_variable({literal,Value,V_Name},Formula={formula,Variable_Dict,Clauses}) ->
    Current_Assignment = dict:fetch(V_Name,Variable_Dict),
    case ternary_logic:xor3(Current_Assignment,Value) of
        indetermined %% The variable is not set for Formula - set it
        	->	%% io:format("~n~nSetting Unit Literal = ~p~n~n",[{literal,V_Name,Value}]),
    			New_V_Dict = dict:store(V_Name,Value,Variable_Dict),
				#formula{variable_dict=New_V_Dict,clauses=Clauses};
        false %% The variable is already set to Value - do nothing
        	->	Formula;
    	true %% The variable is already set to the opposite of Value - this formula is not satisfiable
			->	#formula{variable_dict=Variable_Dict,clauses=[?UNSAT_CLAUSE]}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%								Local Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a literal which is the negation of the literal provided.
%%	In propositional calc this would be written
%%	~p becomes p and p becomes ~p
%%		Literal.  The literal to be negated
%%			#literal
%%
negate_literal({literal,Negation,L_Name}) ->
	#literal{negation=not(Negation),l_name=L_Name}.


%%
%%	Returns a consistent formula made from the (possibly) inconsistent one provided
%%	The formula provided is assumed to have had assignments made to its variables which
%%	have not yet been checked against its constituent clauses.
%%
%%	The returned formula will have had any clauses with at least one literal evaluating to true
%%	removed from its clausal list as they are considered subsumed.  Any literals evaluating to 
%%	false will be removed from the clause in which they appear.
%%	A clause which contains no literals is considered empty and cannot be satisfied from the current assignment.
%%
check_clauses(Formula={formula,Variable_Dict,Clauses}) ->
    {New_Clauses,Unit_Literals} = check_clauses(Formula,Clauses,[],[]),
	{#formula{variable_dict=Variable_Dict,clauses=New_Clauses},Unit_Literals}.

%%
%%	
%%
check_clauses(_Formula,[],New_Clauses,Unit_Literals) ->
    {New_Clauses,Unit_Literals};
    
check_clauses(Formula,[Clause|Clauses],New_Clauses,Unit_Literals) ->
    {New_Clause,Unit_Literal} = check_clause(Formula,Clause),
    case {New_Clause,Unit_Literal} of
        {subsumed,?NULL_LITERAL}
        	->	check_clauses(Formula,Clauses,New_Clauses,Unit_Literals);
        {New_Clause,?NULL_LITERAL}
        	->	check_clauses(Formula,Clauses,[New_Clause|New_Clauses],Unit_Literals);
        {New_Clause,Unit_Literal}
        	->	check_clauses(Formula,Clauses,[New_Clause|New_Clauses],[Unit_Literal|Unit_Literals])
    end.

%%
%%	Returns either a #clause or the atom 'subsumed'.  If subsumed is returned then at least one 
%%	literal in this clause evaluated to true.  Otherwise the #clause returned is guaranteed to be
%%	consistent with respect to the current values assigned to variables in this formula.
%%
%%	A clause is valid with respect to the values assigned to variables when the clause contains only
%%	literals whose values are indetermined.  If a literal is true, then the clause is subsumed.  If a
%%	literal is false then it is removed from the clause.  If a clause is empty then it is not satisfiable
%%	under the current assigned values.
%%
%%	Formula: The #formula in which this clause appears
%%		#formula
%%	Clause: The #clause to be checked
%%		#clause
%%
check_clause(Formula,Clause) ->
    case are_literals_indetermined(Formula,get_literals(Clause,Formula)) of
        true
        	->	{Clause,?NULL_LITERAL};
        false
          	->	modify_clause(Formula,Clause)
    end.

%%
%%	Returns true if each literal in the clause has a variable whose value is indetermined.
%%	Returns false otherwise.
%%
%%	Formula: The formula who literals we are examining
%%		#formula
%%	Literals: A list of literals to check
%%		[#Literal]
%%
are_literals_indetermined(_Formula,[]) -> true;

are_literals_indetermined(Formula,[Literal|Literals]) ->
    case get_literal_value(Literal,Formula) of
        indetermined
        	->	are_literals_indetermined(Formula,Literals);
    	_True_Or_False
			->	false
    end.

modify_clause(Formula,Clause) ->
	Literals = check_literals(Formula,get_literals(Clause,Formula)),
	case Literals of
        subsumed
        	->	{subsumed,?NULL_LITERAL};
        [Literal|[]]
			->	%% %% io:format("~n~nClause = ~p~n~nUnit Literal = ~p~n~n",[Clause,Literals]),
                {#clause{literals=Literals},Literal};
		Literals
			-> {#clause{literals=Literals},?NULL_LITERAL}
	end.

check_literals(_Formula,[]) -> [];

check_literals(Formula,[Literal|Literals]) ->
	case eval_literal(Literal,Formula) of
			true 
				-> subsumed;
			false
				-> check_literals(Formula,Literals);
			indetermined
				-> Other_Literals = check_literals(Formula,Literals),
				   if	%% We must test here whether some literal has evaluated to true, in this case return subsumed
				   		Other_Literals == subsumed
				   			-> subsumed;
						true
				   			-> [Literal|Other_Literals]
				   end
	end.
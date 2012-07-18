-module(formula_generator).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("sat_records.hrl").
-include("test_records.hrl").

-export([generate_formula/2,generate_formula/3,generate_formulae/3,generate_formulae/4]).

%%
%%	Constants
%%
-define(VAR_NUM,"Var_Num").
-define(CLAUSE_NUM,"Clause_Num").
-define(CLAUSE_SIZE,"Clause_Size").

%%
%%	Returns a list of #generated_formula where each formula has random clause size
%%		Var_Num:
%%			The number of variables available to construct the formulae
%%		C_Num:
%%			The number of clauses in each formula (the size of each clause is random 1 <= |C| <= Var_Num)
%%		F_Num:
%%			The number of formulae to generate - F_Num >= 0
%%
generate_formulae(_Var_Num,_C_Num,0) -> [];
	
generate_formulae(Var_Num,C_Num,F_Num) -> 
	[generate_formula(Var_Num,C_Num)|generate_formulae(Var_Num,C_Num,F_Num-1)].

%%
%%	Returns a list of #generated_formula where each formula has fixed clause size
%%		Var_Num:
%%			The number of variables available to construct the formulae
%%		C_Num:
%%			The number of clauses in each formula (clause size is fixed |C| = C_Size)
%%		F_Num:
%%			The number of formulae to generate - F_Num >= 0
%%		C_Size:
%%			The size of each clause in each formula
%%
generate_formulae(_Var_Num,_C_Num,_C_Size,0) -> [];

generate_formulae(Var_Num,C_Num,C_Size,F_Num) -> 
	[generate_formula(Var_Num,C_Num,C_Size)|generate_formulae(Var_Num,C_Num,C_Size,F_Num-1)].

%%
%%	Returns a #generated_formula where the formula has random clause size
%%		Var_Num:
%%			The number of variables available to construct the formula
%%		C_Num:
%%			The number of clauses in each formula (the size of each clause is random 1 <= |C| <= Var_Num)
%%
generate_formula(Var_Num,C_Num) ->
    SVar_Num = process_arg(Var_Num),
    SC_Num = process_arg(C_Num),
    Formula_Def = generate_formula_int(SVar_Num,SC_Num),
    Creational_Metadata = dict:new(),
	Creational_Metadata1 = dict:store(?VAR_NUM,Var_Num,Creational_Metadata),
	Creational_Metadata2 = dict:store(?CLAUSE_NUM,C_Num,Creational_Metadata1),
    #generated_formula{	formula_def=Formula_Def,
						creational_metadata=Creational_Metadata2,
						print_generated_formula={formula_util,print_generated_formula}}.

%%
%%	Returns a #generated_formula where the formula has fixed clause size
%%	The arguments are all either single integers or a list of integers.  Where
%%	an argument is provided as a list of integers one of the values in the list
%%	will be chosen at random.
%%
%%		Var_Num: The number of variables available to construct the formula, or a list of such numbers.
%%			Integer() | [Integer()]
%%		C_Num: The number of clauses in the formula (clause size is fixed |C| = C_Size,C_Size <= Var_Num),
%%			   or a list of such numbers.
%%			Integer() | [Integer()]
%%		C_Size: The size of each clause in the formula, or a list of such numbers.
%%			Integer() | [Integer()]
%%
generate_formula(Var_Num,C_Num,C_Size) ->
    SVar_Num = process_arg(Var_Num),
    SC_Num = process_arg(C_Num),
    SC_Size = process_arg(C_Size),
	Formula_Def = generate_formula_int(SVar_Num,SC_Num,SC_Size),
    Creational_Metadata = dict:new(),
	Creational_Metadata1 = dict:store(?VAR_NUM,SVar_Num,Creational_Metadata),
	Creational_Metadata2 = dict:store(?CLAUSE_NUM,SC_Num,Creational_Metadata1),
	Creational_Metadata3 = dict:store(?CLAUSE_SIZE,SC_Size,Creational_Metadata2),
    #generated_formula{	formula_def=Formula_Def,
						creational_metadata=Creational_Metadata3,
						print_generated_formula={formula_util,print_generated_formula}}.

%%
%%	Returns a single term().  If Arg is a list a random
%%	element is selected from the list and returned.  Otherwise
%%	Arg is returned directly.
%%
%%	Arg: The list or single term to be processed
%%		[Term()] | Term()
%%
process_arg(List) when is_list(List) ->
    lists:nth(random:uniform(length(List)),List);

process_arg(Integer) when is_integer(Integer) ->
    Integer.


%%*******************%%
%%	Local functions	 %%
%%*******************%%

%%
%%	Returns a #formula with random clause size
%%		Var_Num:
%%			The number of variables available to construct the formula
%%		C_Num:
%%			The number of clauses in each formula (the size of each clause is random 1 <= |C| <= Var_Num)
%%
generate_formula_int(Var_Num,C_Num) ->
	L_Names = generate_variables(Var_Num),
	#formula_def{clause_defs=create_clauses(L_Names,C_Num)}.

%%
%%	Returns a #formula with fixed clause size
%%		Var_Num:
%%			The number of variables available to construct the formula
%%		C_Num:
%%			The number of clauses in the formula (clause size is fixed |C| = C_Size,C_Size <= Var_Num)
%%		C_Size:
%%			The size of each clause in the formula
%%
generate_formula_int(Var_Num,C_Num,C_Size) ->
	L_Names = generate_variables(Var_Num),
	#formula_def{clause_defs=create_clauses(L_Names,C_Size,C_Num)}.

%%
%%	Generates a list of clauses of fixed size
%%		L_Names:
%%			A list of variables for literal construction
%%		C_Size:
%%			The size of each clause
%%		C_Num:
%%			The number of clauses to construct
%%
create_clauses(_L_Names,_C_Size,0) -> [];

create_clauses(L_Names,C_Size,C_Num) ->
	[create_clause(L_Names,C_Size)|create_clauses(L_Names,C_Size,C_Num-1)].

%%
%%	Generates a list of clauses of random size
%%		L_Names:
%%			A list of variables for literal construction
%%		C_Num:
%%			The number of clauses to construct (the size of each clause is random 1 <= |C| <= |L_Names|)
%%
create_clauses(_L_Names,0) -> [];

create_clauses(L_Names,C_Num) ->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
	[create_clause(L_Names,random:uniform(length(L_Names)))|create_clauses(L_Names,C_Num-1)].

%%
%%	Creates a clause of disjunctions of literals
%%		L_Names:
%%			A list of variables for literal construction
%%		C_Size:
%%			The size of the clause to be constructed (1 <= C_Size <= |L_Names|)
%%
create_clause(L_Names,C_Size) ->
	#clause_def{literal_defs=lists:map(fun create_literal/1,util:random_sublist(C_Size,L_Names))}.

%%
%%	Constructs a literal (record see sat_records.hrl)
%%		L_Name:
%%			The name of the variable used for this literal
%%
create_literal(L_Name) ->
	#literal_def{negation=util:int_to_bool(random:uniform(2)-1),l_name=L_Name}.

%%
%%	Generates a list distinct names
%%		Var_Num:
%%			The number of variables to produce
%%
generate_variables(0) -> [];

generate_variables(Var_Num) ->
	["X"++util:int_to_string(Var_Num)|generate_variables(Var_Num-1)].
%% Author: Francis Stephens
%% Created: 2 Dec 2008
%% Description: TODO: Add description to formula_util
-module(formula_util).

%%
%% Include files
%%

-include("sat_records.hrl").
-include("test_records.hrl").
-include("process_records.hrl").
-include("cnf_formula_records.hrl").

%%
%% Exported Functions
%%
-export([
         	print_generated_formula/2,invert_negation/1,
            make_formulae_file/6,consult_formulae_file/2,
            clause_def_2_clause/1,literal_def_2_literal/1,make_v_dict/1
        ]).

%%
%% API Functions
%%

%%
%%	The default function for printing a #generated_formula
%%
print_generated_formula(IO_Device,Generated_Formula) ->
	Creational_Metadata = Generated_Formula#generated_formula.creational_metadata,
	Keys = dict:fetch_keys(Creational_Metadata),
	Get_Key_Value = fun(Key) -> [Key,dict:fetch(Key,Creational_Metadata)] end,
	Columns = lists:map(Get_Key_Value,Keys),
	metric_util:r_format_lists(Columns,IO_Device),
	%% The formula definition lines are preceded by the # character so that R's read.table will treat it as comments
	Formula_Def = Generated_Formula#generated_formula.formula_def,
	io:format(IO_Device,"~n~s~n",["# Formula Def"]),
	io:format(IO_Device,"# ~p",[Formula_Def]).

%%
%%	Returns a #formula_def identical to the #formula_def provided but where every literal has been negated.
%%		Formula_Def: The formula_def to be inverted
%%			#formula_def
%%
invert_negation(Formula_Def) ->
	Clause_Defs = Formula_Def#formula_def.clause_defs,
	Invert_Literals = fun(Literal_Def) ->
						#literal_def{	negation = not Literal_Def#literal_def.negation,
										l_name = Literal_Def#literal_def.l_name
									}
					  end,
	Invert_Clauses = fun(Clause_Def) ->
						Literal_Defs = Clause_Def#clause_def.literal_defs,
						#clause_def{literal_defs=lists:map(Invert_Literals,Literal_Defs)}
					 end,
	#formula_def{clause_defs=lists:map(Invert_Clauses,Clause_Defs)}.

%%
%%	Makes a new file containing a list of randomly generated #generated_formula.
%%	
%%	Var_Num: The number of variables in the generated formulae
%%		Integer() | [Integer()]
%%	C_Num: The number of clauses in the generated formulae
%%		Integer() | [Integer()]
%%	C_Size: The size of each clause in the generated_formulae
%%		Intger() | [Integer()]
%%	F_Num: The number formulae to generate
%%		Integer()
%%	File_Name: The name of the file
%%		String()
%%	File_Path: The path of the file, if File_Path =:= [] then the file is 
%%			   created in the directory that Erlang is running from
%%		String()
%%
make_formulae_file(Var_Num,C_Num,C_Size,F_Num,File_Name,File_Path) ->
    Formulae = formula_generator:generate_formulae(Var_Num,C_Num,C_Size,F_Num),
    {ok,File} = util:make_file(File_Name,File_Path),
	io:format(File,"~p.",[Formulae]),
    file:close(File).

%%
%%	Extracts the Erlang terms contained in the file indicated.  While this function's name
%%	suggests that it can only be used for retrieving lists of #generated_formula this is never
%%	checked.  If the file contains other rubbish something bad will happen to your program unless
%%	you are expecting the rubbish that is in the file.
%%
%%	File_Name: The name of the file to be opened
%%		String()
%%	File_Path: The path of the file, if File_Path =:= [] then the file is 
%%			   opened from the directory that Erlang is running in
%%		String()
%%
consult_formulae_file(File_Name,File_Path) ->
    {ok,File} = file:script(File_Path ++ "/" ++ File_Name),
    File.

%%
%%	Returns a new clause constructed from a simple conversion of the #clause_def provided.
%%
%%	Clause_Def: The clause definition to be converted
%%		#clause_def
%%
clause_def_2_clause({clause_def,Literal_Defs}) ->
    Literals = lists:map(fun literal_def_2_literal/1,Literal_Defs),
    #clause{literals=Literals}.

%%
%%	Returns a new literal constructed from a simple conversion of the #literal_def provided.
%%
%%	Literal_Def: The clause definition to be converted
%%		#literal_def
%%
literal_def_2_literal({literal_def,Negation,L_Name}) ->
    #literal{negation=Negation,l_name=L_Name}.

%%
%%	Returns a dictionary mapping variable names to the value indetermined using
%%	the variable names found in the clauses provided.
%%		Clauses
%%			[#clause]
%%
make_v_dict([]) -> dict:new();

make_v_dict([{clause,Literals}|Clauses]) ->
	dict:merge(fun (_K,V1,_V2) -> V1 end,make_variables(Literals),make_v_dict(Clauses)).

%%
%%	Returns a dictionary mapping variable names to the vlaue indetermined using
%%	the variable names found in the literals provided.
%%		Literals
%%			[#literal]
%%
make_variables([]) ->
	dict:new();
	
make_variables([{literal,_Negation,L_Name}|Literals]) ->
	dict:store(L_Name,indetermined,make_variables(Literals)).
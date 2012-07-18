%% Author: Francis Stephens
%% created: 7 Dec 2007
%% Description: Header file containing a collection of hand-built cNF formulae with known properties
%% TODO these are all invalid

%%
%%	Reference Formula
%%
%%		[{clause_def,[{literal_def,false,name1,true}]}].
%%

%%
%%	includes
%%
-include("sat_macros.hrl").

%%
%%	exported functions
%%
-compile(export_all).

%%
%%	Collection of satisfiable formulae
%%
sat1() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1}]}
            ]
        }.
		

sat2() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1},{literal_def,true,name2}]}
            ]
        }.

sat3() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1},{literal_def,true,name2}]},
			{clause_def,[{literal_def,true,name3},{literal_def,false,name1}]}
            ]
        }.

sat4() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1},{literal_def,true,name2}]},
			{clause_def,[{literal_def,false,name1},{literal_def,true,name3}]},
			{clause_def,[{literal_def,true,name4},{literal_def,false,name3}]},
			{clause_def,[{literal_def,false,name4},{literal_def,false,name5}]}
            ]
        }.

sat5() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1},{literal_def,true,name2}]},
			{clause_def,[{literal_def,false,name1},{literal_def,true,name3}]},
			{clause_def,[{literal_def,true,name4},{literal_def,false,name3}]},
			{clause_def,[{literal_def,false,name4},{literal_def,false,name5}]},
			{clause_def,[{literal_def,false,name4},{literal_def,false,name5}]}
            ]
        }.

satisfiable_formulae() ->
	[
	 	sat1(),
		formula_util:invert_negation(sat1()),
	 	sat2(),
		formula_util:invert_negation(sat2()),
	 	sat3(),
		formula_util:invert_negation(sat3()),
		sat4(),
		formula_util:invert_negation(sat4())
	]. 

%%
%%	Collection of unsatisfiable formulae
%%

unsat1() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1},{literal_def,true,name2}]},
			{clause_def,[{literal_def,false,name1},{literal_def,false,name2}]},
			{clause_def,[{literal_def,false,name1},{literal_def,true,name2}]},
			{clause_def,[{literal_def,true,name1},{literal_def,false,name2}]}
            ]
        }.

unsat2() -> 
		{formula_def,[
			{clause_def,[{literal_def,true,name1},{literal_def,true,name2}]},
			{clause_def,[{literal_def,true,name3},{literal_def,false,name1}]},
	   		{clause_def,[{literal_def,false,name1},{literal_def,false,name3}]},
			{clause_def,[{literal_def,false,name2},{literal_def,true,name1}]}
            ]
        }.

unsat3() -> 
		{formula_def,[
			{clause_def,[{literal_def,false,name2},{literal_def,false,name5}]},
			{clause_def,[{literal_def,false,name2},{literal_def,true,name3}]},
			{clause_def,[{literal_def,true,name4},{literal_def,false,name3}]},
			{clause_def,[{literal_def,true,name2},{literal_def,false,name5}]},
			{clause_def,[{literal_def,false,name4},{literal_def,true,name5}]},
			{clause_def,[{literal_def,false,name2},{literal_def,true,name5}]},
			{clause_def,[{literal_def,true,name2},{literal_def,true,name5}]}
            ]
        }.

unsatisfiable_formulae() ->
	[
		unsat1(),
		formula_util:invert_negation(unsat1()),
		unsat2(),
		formula_util:invert_negation(unsat2()),
		unsat3(),
		formula_util:invert_negation(unsat3())
	].
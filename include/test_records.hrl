%% Author: Francis Stephens
%% Created: 23 May 2008
%% Description: TODO: Add description to new_file

%%
%% Records
%%

%%%
%%%	A formula_def provides a definition for a CNF formula
%%%	It lacks any assignment values, but these can be assumed to all be indetermined.
%%%	This is a convenient intermediary format for formula generators and parses.
%%%
%%%	clauses, list of clauses, where each clause is (implicitly) connected by the conjunction (logical and) operator.
%%%		[#clause_def]
%%%
-record(formula_def, {clause_defs}).

%%%
%%%	A clause_def provides a definition for a clause in a CNF formula.
%%%	A clause in a CNF formula is a collection of literals (implicitly)
%%%	connected by the disjunction (logical or) operator.
%%%	
%%%	literal_defs, list of literals
%%%		[#literal_def]
%%%
-record(clause_def, {literal_defs}).

%%%
%%%	A literal_def is a variable, l_name, with a possible assignment and a possible negation
%%%	The negation value acts as a truth switch as follows:
%%%		For a literal, L, where the variable l_name's assignment (val) != indetermined
%%%		truth(L) = (negation & val) v (~negation & ~val)
%%%
%%%	Literals can be read as classical boolean literals
%%%	For some literal named p 
%%%		where negation = true read p
%%%		where negation = false read ~p
%%%
%%% negation
%%%		true | false
%%%	l_name 
%%%		Atom()
-record(literal_def, {negation,l_name}).

%%%
%%%	A test_formula is a formula, usually with a known satisfiability value.
%%%	A test formula may be known satisfiable (true) known unsatisfiable 
%%%	(false) or (less usefully) of unknown satisfiability (indetermined)
%%%
%%%	formula
%%%		#formula_def
%%%
%%%	satisfiable
%%%		true | false | indetermined
%%%
-record(test_formula, {formula_def, satisfiable}).

%%%
%%%	A test_data is a list of #test_formula with a unique id
%%%	allowing for easy database storage.
%%%
%%%	test_id: 	Unique id for this test_data.  Must be unique w.r.t. the database from which
%%%				this test_data is stored and retrieved.
%%%		Integer()
%%%	test_formulae:	A list of #test_formula constituting the body of this test data
%%%		[#test_formula]
%%%
-record(test_data, {test_id,test_formulae}).
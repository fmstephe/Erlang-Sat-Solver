%% Author: Francis Stephens
%% Created: 18 Mar 2008
%% Description: TODO: Add desciption to boolean_expression
-module(boolean_expression).

%%
%%	Include files
%%

-include("sat_records.hrl").
-include("test_records.hrl").

%%
%%	Records
%%

%%%
%%%	A unary expression is a unary boolean expression represented as a
%%%	negation and a variable name or a negation and a complex expression.
%%%
%%%	In the propositional calculus where
%%%		negation is false and var is p we would write
%%%			~p
%%%		negation is true and var is q we would write
%%%			q
%%%
%%%		negation
%%%			true | false
%%%		var
%%%			atom() | #complex_expression
-record(unary_exp,{negation,var}).

%%%
%%%	A complex expression is a binary boolean expression represented as a 
%%%	negation, connective, a left subexpression and right subexpression.
%%%
%%%	In the propositional calculus where 
%%%		connective is con, negation is false, left_exp is p (some expression or atom),
%%%		right_neg is true and right_exp is q (some expression or atom) 
%%%		we would write ~(p & q)
%%%
%%%		negation
%%%			true | false
%%%		connective.  A boolean operator (imp,con,dis for implication conjunction and disjunction respectively).
%%%			imp | con | dis
%%%		left_exp.  The left hand subexpression
%%%			#unary_exp | #complex_exp
%%%		right_exp.  The right hand subexpression
%%%			#unary_exp | #complex_exp
%%%
-record(complex_exp,{negation,connective,left_exp,right_exp}).

%%
%%	Console Functions
%%
-export([parse_expression/1,print_expression/1]).

%% Creational Functions
-export([export_expression/1,make_random_tautology/2,make_random_unsat/2]).

%% Modifying Functions

%%
%%	API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Creational Functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a boolean expression recursively composed of complex_expression and unary_expression records.
%%	The expression string must be a well formed formula (wff) according to the following definition.
%%	
%%		1: Any propositional variable, e.g. p, is well formed.
%%		2: Where p and q are well formed each of:
%%			i)   (p->q)
%%			ii)  (p&q)
%%			iii) (p|q)
%%		  are all well formed.
%%		3: Where p is well formed ~p is well formed
%%
%%	As a relaxation of the definition given above we allow that the two outer most subexpressions, p and q,
%%	of a complex expression do not need to be placed within braces meaning that both (p&q) and p&q are well 
%%	formed.
%%
%%	Currently this function does not handle repeated negations such as ~~p or ~(~p), double negations must 
%%	be resolved by hand at this stage.
%%
%%	Exp_String, string encoding a boolean expression that conforms to well formedness described above
%%		String()
%%
parse_expression(Exp_String) ->
	WF_Exp_String = pre_format(Exp_String),
	parse_wff(WF_Exp_String).
	

parse_wff(WF_Exp_String) ->
	case cut_exp(WF_Exp_String) of
		{Negation,Connective,L_String,R_String}
			-> make_complex(Negation,Connective,parse_wff(L_String),parse_wff(R_String));
		{Negation,Var}
			-> make_unary(Negation,Var)
	end.

%%
%%	Returns a randomly constructed boolean expression.
%%	The variables are constructed from a constant pool of variables.
%%	There is no guarantee that all of the variables available will be used.
%%	
%%		Var_Num, The number of variables in the constant variable pool
%%			Integer
%%		Con_Num, The number of connectives that will make up the resulting expression
%%			Integer
%%
make_random_expression(Var_Num,0) ->
	Negation = util:int_to_bool(random:uniform(2)-1),
	Var = make_random_variable(Var_Num),
	#unary_exp{negation=Negation,var=Var};

make_random_expression(Var_Num,Con_Num) ->
	append_random_expression(Var_Num,Con_Num,make_random_expression(Var_Num,0)).

%%
%%	Returns a randomly constructed boolean expression which is guaranteed unsatisfiable.
%%	The variables are constructed from a constant pool of variables.
%%	There is no guarantee that all of the variables available will be used.
%%
%%	The arguments semantics differs importantly from those of make_random_expression/2.
%%	The arguments relate to the 'schematic components' (The As and Bs below) only not to
%%	the entire expression.
%%	Because of this the arguments are not a good indication of the exact size of the 
%%	expression produced, which may be either 2 or 4 times larger.
%%
%%	The expressions will be of the (guaranteed unsatitisfiable) form of either
%%		1: (A | ~A) > (B & ~B)
%%		2: A & ~A
%%		3: ~(tautology)
%%
%%		Var_Num, The number of variables in the constant variable pool
%%			Integer
%%		Con_Num, The number of connectives that will make up each of the 'schematic components'
%%			Integer
%%
make_random_unsat(Var_Num,Con_Num) ->
	case random:uniform(3) of
		1
			->	A = make_random_expression(Var_Num,Con_Num),
				Not_A = negate(A),
				B = make_random_expression(Var_Num,Con_Num),
				Not_B = negate(B),
				LHS = make_disjunction(true,A,Not_A),
				RHS = make_conjunction(true,B,Not_B),
				make_implication(true,LHS,RHS);
		2
			->	A = make_random_expression(Var_Num,Con_Num),
				Not_A = negate(A),
				make_conjunction(true,A,Not_A);
		3
			->	Tautology = make_random_tautology(Var_Num,Con_Num),
				negate(Tautology)
	end.

%%
%%	Returns a randomly constructed boolean expression which is guaranteed satisfiable.
%%	The variables are constructed from a constant pool of variables.
%%	There is no guarantee that all of the variables available will be used.
%%
%%	The arguments semantics differs importantly from those of make_random_expression/2.
%%	The arguments relate to the 'schematic components' (The As and Bs below) only not to
%%	the entire expression.
%%	Because of this the arguments are not a good indication of the exact size of the 
%%	expression produced, which may be either 2, 3 or 4 times larger.
%%
%%	The expressions will be of the (guaranteed satitisfiable) form of either
%%		1: (A & ~A) > B
%%		2:  A > (B |~ B)
%%		3: ~A | A
%%		4: ~(unsatisfiable expression)
%%
%%		Var_Num, The number of variables in the constant variable pool
%%			Integer
%%		Con_Num, The number of connectives that will make up each of the 'schematic components'
%%			Integer
%%
make_random_tautology(Var_Num,Con_Num) ->
	case random:uniform(4) of
		1
			->	A = make_random_expression(Var_Num,Con_Num),
				Not_A = negate(A),
				B = make_random_expression(Var_Num,Con_Num),
				LHS = make_conjunction(true,A,Not_A),
				make_implication(true,LHS,B);
		2
			->	A = make_random_expression(Var_Num,Con_Num),
				B = make_random_expression(Var_Num,Con_Num),
				Not_B = negate(B),
				RHS = make_disjunction(true,B,Not_B),
				make_implication(true,A,RHS);
		3
			->	A = make_random_expression(Var_Num,Con_Num),
				Not_A = negate(A),
				make_disjunction(true,A,Not_A);
		4
			->	Unsat = make_random_unsat(Var_Num,Con_Num),
				negate(Unsat)
	end.

%%
%%	Returns a randomly constructed boolean expression by appending a new expression to the one provided.
%%	The variables are constructed from a constant pool of variables.
%%	There is no guarantee that all of the variables available will be used.
%%
%%		Var_Num, The number of variables in the constant variable pool
%%			Integer
%%		Con_Num, The number of connectives that will make up the resulting expression
%%			Integer	
%%		Expression, A boolean expression for the LHS of the constructed expression
%%			#complex_exp | #unary_exp
%%
append_random_expression(_Var_Num,0,Expression) ->
	Expression;

append_random_expression(Var_Num,Con_Num,Left_Exp) ->
	Right_Cons = random:uniform(Con_Num)-1, 			%% Number of connectives in the right hand expression
	Remaining_Cons = Con_Num-Right_Cons-1,				%% Number of connectives remaining to be used
	Right_Exp = make_random_expression(Var_Num,Right_Cons),
	Negation = util:int_to_bool(random:uniform(2)-1),
	Connective = make_random_connective(),
	New_Exp = #complex_exp{negation=Negation,connective=Connective,left_exp=Left_Exp,right_exp=Right_Exp},
   	append_random_expression(Var_Num,Remaining_Cons,New_Exp).

%%
%%	Returns a variable name selected randomly from a set of Var_Num distinct variable names
%%
%%		Var_Num, the number of distinct variables names from which to choose
%%			Integer
%%
make_random_variable(Var_Num) ->
	"V"++util:int_to_string(random:uniform(Var_Num)).

%%
%%	Returns either con, dis or imp atoms chosen at random.
%%
make_random_connective() ->
	case random:uniform(3) of
		1
			-> con;
		2
			-> dis;
		3
			-> imp
	end.

%%
%%	Returns a unary boolean expression
%%		Var. The literal name
%%			Atom()
%%		Neg. The negation of this literal
%%			true | false
%%
make_unary(Negation,Var) ->
	#unary_exp{negation=Negation,var=Var}.

%%
%%	Returns a new binary boolean expression using two subexpressions with a boolean connective
%%		Connective.   Boolean operator for this expression.  Unary is not a legal value.
%%			imp | dis | con
%%		Left_Neg. Negation of the left hand subexpression
%%			true | false
%%		Left_Rand.  The left hand subexpression
%%			Atom() | #bool_formula
%%		Right_Neg. Negation of the right hand subexpression
%%			true | false
%%		Right_Rand.  The right hand subexpression
%%			Atom() | #bool_formula
%%
make_conjunction(Negation,Left_Exp,Right_Exp) ->
	make_complex(Negation,con,Left_Exp,Right_Exp).

make_disjunction(Negation,Left_Exp,Right_Exp) ->
	make_complex(Negation,dis,Left_Exp,Right_Exp).

make_implication(Negation,Left_Exp,Right_Exp) ->
	make_complex(Negation,imp,Left_Exp,Right_Exp).

make_complex(Negation,Connective,Left_Exp,Right_Exp) ->
	#complex_exp{negation=Negation,connective=Connective,left_exp=Left_Exp,right_exp=Right_Exp}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Enquiry Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns true iff the binary expression provided is a unary expression.
%%		Boolean_Formula
%%			#bool_formula
%%
is_unary({unary_exp,_Negation,_Var}) -> true;

is_unary({complex_exp,_Negation,_Connective,_L_Exp,_R_Exp}) -> false.

%%
%%	Returns true iff the binary expression provided is a binary expression
%%	Binary expressions use one of the connectives imp | and | or.
%%		Boolean_Formula
%%			#bool_formula
%%
is_complex({unary_exp,_Negation,_Var}) -> false;

is_complex({complex_exp,_Negation,_Connective,_L_Exp,_R_Exp}) -> true.

%%
%%	Returns true iff the expression provided contains an instance of the connective searched for
%%
%%		Expression, the expression un which to search for the connective
%%			#complex_exp | #unary_exp
%%		Test_Con, the connective to search for
%%			con | dis | imp
%%
contains_connective({complex_exp,_Negation,con,_L_Exp,_R_Exp},con) -> true;

contains_connective({complex_exp,_Negation,dis,_L_Exp,_R_Exp},dis) -> true;

contains_connective({complex_exp,_Negation,imp,_L_Exp,_R_Exp},imp) -> true;

contains_connective({complex_exp,_Negation,_Connective,L_Exp,R_Exp},Test_Con) ->
	contains_connective(L_Exp,Test_Con) or contains_connective(R_Exp,Test_Con);

contains_connective({unary_exp,_Negation,_Var},_Test_Con) ->
	false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Modifying Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nnf(Formula={unary_exp,_Negation,_Var}) ->
	Formula;

nnf({complex_exp,Negation,imp,L_Exp,R_Exp}) ->
	nnf(#complex_exp{negation=Negation,connective=dis,left_exp=negate(L_Exp),right_exp=R_Exp});	%%In this case we remove the implication

nnf({complex_exp,true,Connective,L_Exp,R_Exp}) ->
	#complex_exp{negation=true,connective=Connective,left_exp=nnf(L_Exp),right_exp=nnf(R_Exp)};

nnf(Negated_Formula) ->
	Unnegated_Formula = propogate_neg(Negated_Formula),
	nnf(Unnegated_Formula).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%						Presentational Functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_expression({complex_exp,true,Connective,Left_Exp,Right_Exp}) ->
	P = append_exp_string(Left_Exp,""),
	PC = append_connective(Connective,P),
	PCQ = append_exp_string(Right_Exp,PC),
	lists:reverse(PCQ);
	
print_expression(Complex_Expression = {complex_exp,false,_Connective,_Left_Exp,_Right_Exp}) ->
	lists:reverse(append_exp_string(Complex_Expression,""));

print_expression(Unary_Expression = {unary_exp,_Negation,_Variable}) ->
	lists:reverse(append_exp_string(Unary_Expression,"")).

append_exp_string({complex_exp,true,Connective,Left_Exp,Right_Exp},String_Exp) ->
	P = append_exp_string(Left_Exp,[$(|String_Exp]),
	PC = append_connective(Connective,P),
	PCQ = append_exp_string(Right_Exp,PC),
	[$)|PCQ];

append_exp_string({complex_exp,false,Connective,Left_Exp,Right_Exp},String_Exp) ->
	P = append_exp_string(Left_Exp,[$(,$~|String_Exp]),
	PC = append_connective(Connective,P),
	PCQ = append_exp_string(Right_Exp,PC),
	[$)|PCQ];

append_exp_string({unary_exp,true,Variable},String_Exp) ->
	lists:reverse(Variable)++String_Exp;

append_exp_string({unary_exp,false,Variable},String_Exp) ->
	lists:reverse([$~|Variable])++String_Exp.

append_connective(imp,P) ->
	[$ ,$>,$ |P];

append_connective(con,P) ->
	[$ ,$&,$ |P];

append_connective(dis,P) ->
	[$ ,$|,$ |P].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Export Functions								%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%%	Returns a #formula_def that models the boolean expression (#complex_exp or #unary_exp) provided.
%%
%%	Exp, the boolean expression to be exported
%%		#complex_exp | #unary_exp
%%
export_expression(Exp) ->
	CNF_Exp = cnf(nnf(Exp)),
	#formula_def{clause_defs=collect_clauses(CNF_Exp,[])}.

collect_clauses(Unary_Exp={unary_exp,_Negation,_Var},Clauses) ->
	Literals = collect_literals(Unary_Exp,[]),
	New_Clause = #clause_def{literal_defs=Literals},
	[New_Clause|Clauses];
    
collect_clauses({complex_exp,true,con,Left_Exp,Right_Exp},Clauses) ->
	Clauses_Left = collect_clauses(Left_Exp,Clauses),
	collect_clauses(Right_Exp,Clauses_Left);

collect_clauses({complex_exp,true,dis,Left_Exp,Right_Exp},Clauses) ->
	Literals_Left = collect_literals(Left_Exp,[]),
	Literals_Right = collect_literals(Right_Exp,Literals_Left),
	New_Clause = #clause_def{literal_defs=Literals_Right},
	[New_Clause|Clauses].

collect_literals({unary_exp,Negation,Var},Literals) ->
	New_Literal = #literal_def{negation=Negation,l_name=Var},
	[New_Literal|Literals];

collect_literals({complex_exp,true,dis,Left_Exp,Right_Exp},Literals) ->
	Literals_Left = collect_literals(Left_Exp,Literals),
	collect_literals(Right_Exp,Literals_Left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%							Local Functions									%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a string encoding of a boolean expression such that two the outermost subexpressions
%%	of the expression provided are guaranteed to be surrounded by braces.  This is a fixing of
%%	the formatting relaxation allowed by the public expression parsing function, parse_wff/1, given above.
%%
%%	This function ensures that expressions written p&q, where p and q are well formed, is returned {p&q).
%%	expressions of the form (p&q), ~(p&q), r and ~r (where r is a propositional variable) remain unchanged.
%%
%%		Exp_String, string encoding a boolean expression that conforms to well formedness described above in parse_wff/1
%%			String()
%%
pre_format(Exp_String) ->
    F_Exp_String = lists:filter(fun($ )->false;(_)->true end,Exp_String),
	case is_singly_braced(F_Exp_String) or (not has_connective(F_Exp_String)) of
		true
			-> 	F_Exp_String;
		false
			-> 	"(" ++ F_Exp_String ++ ")"
	end.

%%	
%%	Returns true iff the expression string provided contains either
%%		&
%%		|
%%		>
%%	returns false otherwise.
%%
%%		Exp_String, expressions string to be tested for connectives
%%			String()
%%
has_connective(Exp_String) ->
	Con = lists:member($&,Exp_String),
	Dis = lists:member($|,Exp_String),
	Imp = lists:member($>,Exp_String),
	Con or Dis or Imp.

%%
%%	Returns true iff the string encoding of a boolean expression appears in the form
%%		1: r or ~r, where r is a propositional variable
%%		2: (p&q) or ~(p&q), where p and q expresions conforming to well formedness described above in parse_wff/1
%%
%%	Expressions appearing as p&q will return false.
%%
%%		Exp_String, string encoding a boolean expression that conforms to well formedness described above in parse_wff/1
%%			String()
%%
is_singly_braced([$~|Exp_String]) ->
	is_singly_braced(Exp_String);

is_singly_braced([$(|Exp_String]) ->
	is_singly_braced(Exp_String,1);

is_singly_braced(_Exp_String) ->
	false.

is_singly_braced([],0) ->
	true;

is_singly_braced(_Exp_String,0) ->
	false;

is_singly_braced([$(|Exp_String],Count) ->
	is_singly_braced(Exp_String,Count+1);

is_singly_braced([$)|Exp_String],Count) ->
	is_singly_braced(Exp_String,Count-1);

is_singly_braced([_Char|Exp_String],Count) ->
	is_singly_braced(Exp_String,Count).

%%
%%	Returns a tuple of the form {Negation,Connective,Left_String,Right_String} where:
%%		Negation, Indicates whether the expression cut is negated
%%			true | false
%%		Connective, Indicates the binary operator connecting the left and right subexpressions
%%			dis, con, imp
%%		Left_String, The (unprocessed) string encoding for the left hand subexpression
%%			String()
%%		Right_String, The (unprocessed) string encoding for the right hand subexpression
%%			String()
%%	
%%	The four place tuple described above is returned for encodings containing a bianry operator such that:
%%		The expression (p&q) would return
%%			{true,con,p,q}
%%		The expression ~(q->q) would return
%%			{false,imp,p,q}
%%	
%%	Alternately a tuple of form {Negation,Variable} where
%%		Negation, Indicates whether the sole variable in the expression is negated
%%			true | false
%%		Variable, The name of a propositional variables
%%			atom()
%%
%%	The two place tuple described above is returned for encodings containing a, possibly negated, propositional variable
%%		The expressions r would return
%%			{true,r}
%%		The expression ~r would return
%%			{false,r}
%%
%%		Exp_String_QCP, string encoding a boolean expression that conforms to well formedness described above in parse_wff/1
%%			String()
%%
cut_exp([$~,$(|Exp_String_QCP]) ->
	Negation = false,
	{Left_String,Exp_String_CP} = cut_left_exp(Exp_String_QCP),
	{Connective,Exp_String_P} = cut_connective(Exp_String_CP),
	Right_String = cut_right_exp(Exp_String_P),
	{Negation,Connective,Left_String,Right_String};

cut_exp([$(|Exp_String_QCP]) ->
	Negation = true,
	{Left_String,Exp_String_CP} = cut_left_exp(Exp_String_QCP),
	{Connective,Exp_String_P} = cut_connective(Exp_String_CP),
	Right_String = cut_right_exp(Exp_String_P),
	{Negation,Connective,Left_String,Right_String};

cut_exp([$~|Exp_String_V]) ->
	{false,Exp_String_V};

cut_exp(Exp_String_V) ->
	{true,Exp_String_V}.

%%
%% Returns, for an expression of the form q&p), a tuple of the form {Left_Exp,Exp_String_CP} where
%%		Left_Exp, String encoding of a boolean expression (in this case q)
%%			String()
%%		Exp_String, String encoding of the binary operator connecting q and p and p itself (in this case &p)
%%			String()
%%
%%	cut_left_exp acts a tokeniser dividing a string encoding of a boolean expression before the highest level operator in the expression.
%%
%%		Exp_String_QCP, string encoding a boolean expression that conforms (almost) to well formedness described above in parse_wff/1
%%			String()
%%
cut_left_exp([$~|Exp_String_QCP]) ->
	{Left_Exp,Exp_String_CP} = cut_left_exp(Exp_String_QCP),
	{[$~|Left_Exp], Exp_String_CP};

cut_left_exp(Exp_String_QCP = [$(|_Exp]) ->
	cut_parenthesis(Exp_String_QCP);

cut_left_exp(Exp_String_QCP) ->
	cut_left_atom(Exp_String_QCP).

%%
%%	Returns a tuple of the form {Left_Exp,Exp_String_CP} where
%%		Left_Exp, the contents of the left most parethesis pair in the expression string provided
%%			String()
%%		Exp_String_CP, the rest of the expression string
%%	
%%		Exp_String_QCP, string encoding a boolean expression that conforms (almost) to well formedness described above in parse_wff/1
%%			String()
%%
cut_parenthesis([$(|Exp_String_QCP]) ->
	{Left_Exp,Exp_String_CP} = cut_parenthesis(Exp_String_QCP,1),
	{[$(|Left_Exp],Exp_String_CP}.

cut_parenthesis(Exp_String_CP,0) ->
	{[],Exp_String_CP};

cut_parenthesis([$(|Exp_String_QCP],Open_Count) ->
	{Left_Exp,Exp_String_CP} = cut_parenthesis(Exp_String_QCP,Open_Count+1),
	{[$(|Left_Exp],Exp_String_CP};

cut_parenthesis([$)|Exp_String_QCP],Open_Count) ->
	{Left_Exp,Exp_String_CP} = cut_parenthesis(Exp_String_QCP,Open_Count-1),
	{[$)|Left_Exp],Exp_String_CP};

cut_parenthesis([Char|Exp_String_QCP],Open_Count) ->
	{Left_Exp,Exp_String_CP} = cut_parenthesis(Exp_String_QCP,Open_Count),
	{[Char|Left_Exp],Exp_String_CP}.

%%
%%	Returns the expression string provided with the hanging right-most parenthesis removed.
%%
%%		Exp_String_PB, expression string with a hanging parenthesis at the end
%%
cut_right_exp(Exp_String_PB) ->
	[$)|Exp_String_P] = lists:reverse(Exp_String_PB),
	lists:reverse(Exp_String_P).

%%
%%	Returns, privded an expression of the form r&p) (where r is a propositional variable), a tuple 
%%	of the form {Variable,Exp_String_CP} where
%%		Variable, Propostional variable name
%%			Atom()
%%		Exp_String_CP, String encoding of the binary operator connecting r and p and p itself (in this case &p)
%%			String()
%%
%%		String_QCP,  string encoding a boolean expression that conforms (almost) to well formedness described above in parse_wff/1
%%
cut_left_atom(String_CP = [$>|_Rest]) ->
	{[],String_CP};

cut_left_atom(String_CP = [$&|_Rest]) ->
	{[],String_CP};

cut_left_atom(String_CP = [$||_Rest]) ->
	{[],String_CP};

cut_left_atom([Char|String_QCP]) ->
	{P_Fragment, String_CP} = cut_left_atom(String_QCP),
	{[Char|P_Fragment], String_CP}.

cut_connective([$>|String_P]) -> {imp,String_P};

cut_connective([$&|String_P]) -> {con,String_P};

cut_connective([$||String_P]) -> {dis,String_P}.

%%
%%	Negates an expression
%%
negate({unary_exp,Negation,Var}) ->
	#unary_exp{negation=not Negation,var=Var};

negate({complex_exp,Negation,Connective,Left_Exp,Right_Exp}) ->
	#complex_exp{negation=(not Negation),connective=Connective,left_exp=Left_Exp,right_exp=Right_Exp}.

%%
%%	Propogates a negation
%%	~(p and q) == ~p or ~q
%%	~(p or q) == ~p and ~q
%%
propogate_neg({complex_exp,Negation,imp,L_Exp,R_Exp}) ->
	#complex_exp{negation=not Negation,connective=con,left_exp=L_Exp,right_exp=negate(R_Exp)};

propogate_neg({complex_exp,Negation,dis,L_Exp,R_Exp}) ->
	#complex_exp{negation=not Negation,connective=con,left_exp=negate(L_Exp),right_exp=negate(R_Exp)};

propogate_neg({complex_exp,Negation,con,L_Exp,R_Exp}) ->
	#complex_exp{negation=not Negation,connective=dis,left_exp=negate(L_Exp),right_exp=negate(R_Exp)}.

%%
%%	Expression provided to cnf is assumed to be NNF
%%	Assuming a cnf expression we can take any other cnf Expression and conjoin them
%%	If the sub-expression is cnf we can happily conjoin it to another cnf expression
%%	If we want to disjoin two cnf expression we may only do so if they each contain no conjunctions
%%	Otherwise, we must take one and *distribute* it into the other as in X or (A and B and C) == (A or X) and (B or X) and (C or X)
%%
cnf(Expression={unary_exp,_Negation,_Var}) ->
	Expression;

cnf({complex_exp,true,con,L_Exp,R_Exp}) ->
	#complex_exp{negation=true,connective=con,left_exp=cnf(L_Exp),right_exp=cnf(R_Exp)};

cnf(Expression={complex_exp,true,dis,L_Exp,R_Exp}) ->
	case {contains_connective(L_Exp,con),contains_connective(R_Exp,con)} of
		{_,true}	% This means that we can (and should) distribute into the right subexpression
			->	L_CNF = cnf(L_Exp),
				R_CNF = cnf(R_Exp),
				cnf(dist_into(L_CNF,R_CNF));
		{true,_}	% This means that we can (and should) distribute into the left subexpression
			->	L_CNF = L_Exp,
				R_CNF = cnf(R_Exp),
				cnf(dist_into(R_CNF,L_CNF));
		{false,false}	% This means we are already in cnf (no conjunctions in subformulae)
			->	Expression
	end.

%%	Assumed that both Expression and Into_Expression are CNF
%%	dist_into needs to account for the asymetrical case where an atom appears on one side (we can't distribute into it
%%	dist_into when used once does not produce cnf expression.  We must make the produced subexpression cnf.

dist_into(Expression,{complex_exp,true,con,L_Exp,R_Exp}) ->
	L_Exp_New = #complex_exp{negation=true,connective=dis,left_exp=Expression,right_exp=L_Exp},
	R_Exp_New = #complex_exp{negation=true,connective=dis,left_exp=Expression,right_exp=R_Exp},
	#complex_exp{negation=true,connective=con,left_exp=L_Exp_New,right_exp=R_Exp_New};

dist_into(Expression,{complex_exp,true,dis,L_Exp,R_Exp}) ->
	case {contains_connective(L_Exp,con),contains_connective(R_Exp,con)} of
		{_,true}	% This means that we can (and should) distribute into the right subexpression
			->	{complex_exp,true,dis,L_Exp,dist_into(Expression,R_Exp)};
		{true,_}	% This means that we can (and should) distribute into the left subexpression
			->	{complex_exp,true,dis,dist_into(Expression,L_Exp),R_Exp}
	end.
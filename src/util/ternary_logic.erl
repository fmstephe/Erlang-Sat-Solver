%% Author: Francis Stephens
%% Created: 21 Dec 2007
%% Description: Custom functions for a three valued logic.  This has true false and indetermined.
-module(ternary_logic).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([and3/2,or3/2,xor3/2,not3/1,equal3/2,all_true3/1,one_true3/1,all_false3/1,none_false3/1,none_true3/1,one_false3/1,ind_to_true/1,ind_to_false/1]).

%%
%% API Functions
%%

%%
%% Local Functions
%%

%%
%%	And3 is a ternary logic operator and has a truth table as follows
%% 		NB: ind is short for indetermined
%%
%%		and3
%%				true	false	ind
%%		true	true	false	ind
%%		false	false	false	false
%%		ind		ind		false	ind
%%
and3(indetermined,false) -> false;

and3(indetermined,true) -> indetermined;

and3(false,indetermined) -> false;

and3(true,indetermined) -> indetermined;

and3(indetermined,indetermined) -> indetermined;

and3(A,B) ->
	A and B.


%%
%%	Or3 is a ternary logic operator and has a truth table as follows
%% 		NB: ind is short for indetermined
%%
%%		or3
%%				true	false	ind
%%		true	true	true	true
%%		false	true	false	ind
%%		ind		true	ind		ind
%%
or3(indetermined,true) -> true;

or3(indetermined,false) -> indetermined;

or3(true,indetermined) -> true;

or3(false,indetermined) -> indetermined;

or3(indetermined,indetermined) -> indetermined;

or3(A,B) ->
	A or B.

%%
%%	XOr3 is a ternary logic operator and has a truth table as follows
%% 		NB: ind is short for indetermined
%%
%%		xor3
%%				true	false	ind
%%		true	false	true	ind
%%		false	true	false	ind
%%		ind		ind		ind		ind
%%
xor3(indetermined,true) -> indetermined;

xor3(indetermined,false) -> indetermined;
  
xor3(true,indetermined) -> indetermined;

xor3(false,indetermined) -> indetermined;

xor3(indetermined,indetermined) -> indetermined;

xor3(A,B) -> A xor B.

%%
%%	Equal3 is a ternary logic operator and has a truth table as follows
%% 		NB: ind is short for indetermined
%%
%%		equal3
%%				true	false	ind
%%		true	true	false	ind
%%		false	false	true	ind
%%		ind		ind		ind		ind
%%
equal3(_A,indetermined) -> indetermined;

equal3(indetermined,_B) -> indetermined;

equal3(A,B) ->
    A =:= B.

%%
%%	not3 is a unary ternary logic operator and has truth mapping as follows
%%		NB: ind is short for indetermined
%%
%%		not3
%%		true 	-> 	false
%%		false 	-> 	true
%%		ind		->	ind
%%
not3(true) -> false;

not3(false) -> true;

not3(indetermined) -> indetermined.

%%
%%	all_true3 operates on any list
%%	it is usual for this list to contain a combination of either true, false or indetermined
%%	But any list will work
%%
%%	all_true3 returns true iff every element in Vals is true, false otherwise
%%
all_true3([]) ->
	true;

all_true3([true|Vals]) ->
	all_true3(Vals);

all_true3(_Vals) ->
	false.

%%
%%	all_false3 operates on any list
%%	it is usual for this list to contain a combination of either true, false or indetermined
%%	But any list will work
%%
%%	all_false3 returns true iff every element in Vals is false, false otherwise
%%
all_false3([]) ->
	true;

all_false3([false|Vals]) ->
	all_false3(Vals);

all_false3(_Vals) ->
	false.
	
%%
%%	none_true3 operates on any list
%%	it is usual for this list to contain a combination of either true, false or indetermined
%%	But any list will work
%%
%%	none_true3 returns true iff no element in Vals is true
%%
none_true3(Vals) -> 
	not lists:member(true,Vals).

%%
%%	none_false3 operates on any list
%%	it is usual for this list to contain a combination of either true, false or indetermined
%%	But any list will work
%%
%%	none_false3 returns true iff no element in Vals is false
%%
none_false3(Vals) ->
	not lists:member(false,Vals).

%%
%%	one_true3 operates on any list
%%	it is usual for this list to contain a combination of either true, false or indetermined
%%	But any list will work
%%
%%	one_true3 returns true iff at least one element in Vals is true
%%
one_true3(Vals) -> 
	lists:member(true,Vals).

%%
%%	one_false3 operates on any list
%%	it is usual for this list to contain a combination of either true, false or indetermined
%%	But any list will work
%%
%%	one_false3 returns true iff at least one element in Vals is false
%%
one_false3(Vals) ->
	lists:member(false,Vals).

%%
%%	function converts indetermined into true
%%	other values are unmodified
%%
ind_to_true(indetermined) -> true;

ind_to_true(A) -> A.

%%
%%	function converts indetermined to false
%%	other values are unmodified
%%
ind_to_false(indetermined) -> false;

ind_to_false(A) -> A.
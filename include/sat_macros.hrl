%% Author: Francis Stephens
%% Created: 30 Apr 2008
%% Description: TODO: Add description to sat_macros

%%
%%	Macros
%%

%%%
%%%	Formula module macro
%%%
%%%	This macro is used to universally replace the formula (cnf)
%%%	data structure used throughout the sat solver application.
%%%
-define(FORMULA_DATA_STRUCTURE,cnf_del_cp).

%%%	Formula module macro
%%%
%%%	This macro is used to universally replace the formula (cnf)
%%%	data structure used throughout the sat solver application.
%%%
-define(FORMULA_GRAPH_STRUCTURE,cnf_graph_list).
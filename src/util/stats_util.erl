%% Author: Francis Stephens
%% Created: 8 Jun 2009
%% Description: TODO: Add description to stats_util
-module(stats_util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([min/2,max/2,mean/1,mean_vertical_2d/1,standard_deviation/1]).

%%
%% API Functions
%%

%%
%%	Returns the least of two numerics
%%	The atom infinity is a legal value
%%	as is negative_infinity
%%
min(infinity,Y) ->
	Y;

min(X,infinity) ->
	X;

min(negative_infinity,_Y) ->
	negative_infinity;

min(_X,negative_infinity) ->
	negative_infinity;

min(X,Y) when X < Y ->
	X;

min(_X,Y) -> Y.

%%
%%	Returns the greater of two numerics
%%	The atom infinity is a legal value
%%	as is negative_infinity
%%
max(infinity,_Y) ->
	infinity;

max(_X,infinity) ->
	infinity;

max(negative_infinity,Y) ->
	Y;

max(X,negative_infinity) ->
	X;

max(X,Y) when X > Y ->
    X;

max(_X,Y) -> Y.

%%
%% Returns a Float() which is the mean of Number_List
%%
%%	Numbers_List: A list of numbers whose mean we are curious about
%%		[Number()]
%%
mean(Numbers_List) -> 
	lists:sum(Numbers_List) / length(Numbers_List).

%%
%%	Returns the mean for every column in the 2d matrix
%%
%%	Numbers_Matrix: A 2d matrix of numbers
%%		[[Number()]]
%%
mean_vertical_2d(Numbers_Matrix) ->
	Height = length(Numbers_Matrix),
	Width = length(hd(Numbers_Matrix)),
	Zeroed_columns = util:generate_list(fun() -> 0 end, Width),
	Summed_Rows = lists:foldl(fun(Acc,List) -> util:add_list_elements(Acc,List) end, Zeroed_columns, Numbers_Matrix),
	lists:map(fun(Num) -> Num/Height end, Summed_Rows).

%%
%% Returns a Float() which is the standard deviation of Numbers_List
%%
%%	Number_List: A list of numbers whose standiard diviation we are curious about
%%	[Number()]
%%
standard_deviation(Numbers_List) -> 
	Mean = mean(Numbers_List),
    Var = mean([math:pow((Mean-Num),2)||Num <- Numbers_List]),
    math:sqrt(Var).



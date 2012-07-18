%% Author: Francis Stephens
%% Created: 2 Dec 2007
%% Description: TODO: Add desciption to util
-module(util).

%%
%% Include files
%%

-include("sat_records.hrl").

%%
%% Exported Functions
%%
-export([
		 	value_or_default/2,
            int_to_bool/1,int_to_string/1,format_int/1,
            tail_or_empty/1,heads/1,tails/1,deep_foldl/3,difference_list/1,random_sublist/2,split_alternate/1,split_in_half/1,all_pairs/1,homogenous_list/1,combine_without_duplicates/2,combinate/1,drop_last/1,add_list_elements/2,
            delete_element/2,delete_elements/2,delete_elements_unsafe/2,get_indices/1,
            make_file/2,
            get_time_stamp/0,get_filename_time_stamp/0,
            filter_map/3,map_filter/3,generate_list/2,generate_numbered_list/2,do_n_times/2,foldl_abandon/4,loop_n_times/3
		]).

%%
%% API Functions
%%

%%
%%	Returns the value provided, unless it is 'undefined'
%%	then the Default is returned instead.
%%
%%	Value: The value to be returned, provided it isn't 'undefined'
%%		Term()
%%	Default: The default value to be returned if Value is 'undefined'
%%		Term()
%%
value_or_default(undefined,Default) ->
	Default;

value_or_default(Value,_Default) ->
	Value.

%%
%% Converts integers into boolean values
%% Even numbers are false,odd numbers are true;
%%
int_to_bool(Int) -> 
	Val = Int rem 2,
	if
		Val == 0 -> true;
		true -> false
	end.

int_to_string(0) ->
	"0";

int_to_string(Int) ->
    case Int > 0 of
		true
          	->	lists:reverse(int_to_string(Int,10));
        false
          	->	[45|lists:reverse(int_to_string(abs(Int),10))]
    end.

int_to_string(0,_) -> [];

int_to_string(Int,Div) ->
	[(Int rem Div)+48|int_to_string(trunc(Int/Div),Div)].

%%
%%	Returns a formatted string representation of the integer provided,
%%	only positive numbers >= 0 are handled sensibly by this function
%%	100 	-> 100
%%	1000 	-> 1,000
%%	10000 	-> 10,000
%%	100000	-> 100,000
%%	etc.
%%
%%	
%%
format_int(Int) ->
	String_Int = int_to_string(Int),
    case hd(String_Int) =:= 45 of
        true
       		->	[45|add_commas(tl(String_Int))];
		false
        	->	add_commas(String_Int)
    end.

add_commas(String_Int) ->
	case string:len(String_Int) of
		Len when Len < 4
			->	String_Int;
		Len when Len =:= 4
			->	[A,B,C,D] = String_Int,
				[A,44,B,C,D];
		Len when Len rem 3 =:= 0
			->	[A,B,C|Nums] = String_Int,
				[A,B,C|add_commas_int(Nums)];
		Len when Len rem 3 =:= 1
			->	[A|Nums] = String_Int,
				[A|add_commas_int(Nums)];
		Len when Len rem 3 =:= 2
			->	[A,B|Nums] = String_Int,
				[A,B|add_commas_int(Nums)]
	end.

add_commas_int([]) ->
	[];

add_commas_int([Num|Nums]) ->
	case string:len([Num|Nums]) rem 3 of
		0
			->	[44,Num|add_commas_int(Nums)];
		_Not_Zero
			->	[Num|add_commas_int(Nums)]
	end.

%%
%%	Returns the tail of the list provided (as per erlang:tl/1)
%%	or the empty list if the list provided was empty.
%%
%%	List: The list whose tail we seek
%%		[Term()]
%%
tail_or_empty([]) ->
    [];

tail_or_empty([_Head|Tail]) ->
    Tail.

%%
%%	Returns the head of each list, forom the lists provided, as a list.
%%
%%	Lists: List of lists, where the head of each list will be extracted
%%		[[Term()]]
%%
heads([]) -> [];

heads([List|Lists]) ->
    [hd(List)|heads(Lists)].

%%
%%	Returns the tail of each list, forom the lists provided, as a list.
%%
%%	Lists: List of lists, where the tail of each list will be extracted
%%		[[Term()]]
%%
tails([]) -> [];

tails([List|Lists]) ->
    [tl(List)|tails(Lists)].

%%
%%	
%%
deep_foldl(_Fun,Acc,[]) -> Acc;
    
deep_foldl(Fun,Acc,[List|Deep_List]) when is_list(List) ->
	New_Acc = deep_foldl(Fun,Acc,List),
	deep_foldl(Fun,New_Acc,Deep_List);

deep_foldl(Fun,Acc,[Head|Deep_List]) ->
	deep_foldl(Fun,Fun(Head,Acc),Deep_List).
                                     
%%
%%	Returns a list of Numeric() values which indicate the absolute difference between
%%	each adjacent pair of Numeric() values in the list provided.
%%
%%	List: The list from which the differences are to be computed
%%		[Numeric()]
%%
difference_list([Init|List]) ->
	difference_list(List,Init).

difference_list([],_Compare) ->
	[];

difference_list([Value|Values],Compare) ->
	[abs(Value-Compare)|difference_list(Values,Value)].

%%
%%	Returns a random sublist from the list provided
%%
%%	Sub_Size: The size of the list to be returned
%%		Integer()
%%	List: The list from which a sublist is created
%%		[Term()]
%%
random_sublist(0,_List) ->
	[];

random_sublist(Sub_Size,List) ->
	I = random:uniform(length(List)),
	[lists:nth(I,List)|random_sublist(Sub_Size-1,remove_by_index(I,List))].

%%
%%	Returns a list with the element at the index provided removed.
%%
%%	I: The index at which the element will be removed
%%		Integer()
%%	List: The list whose element will be removed
%%		[Term()]
%%
remove_by_index(1,[_|Tail]) -> Tail;

remove_by_index(I,[H|Tail]) ->
	[H|remove_by_index(I-1,Tail)].

%%
%%	Returns two lists containing the elements of the list
%%	provided distributed between the two lists in an alternating
%%	fashion.
%%	e.g. [1,2,3,4] becomes {[1,3],[2,4]}
%%
%%	List: The list to be split
%%		[Term()]
%%
split_alternate(List) ->
    split_alt([],[],true,List).

split_alt(First,Second,_Ignored,[]) ->
	{lists:reverse(First),lists:reverse(Second)};

split_alt(First,Second,true,[Head|Tail]) ->
    split_alt([Head|First],Second,false,Tail);

split_alt(First,Second,false,[Head|Tail]) ->
    split_alt(First,[Head|Second],true,Tail).

%%
%%	Returns two lists which will be the first and second
%%	halves of the list provided.  Where the list provided
%%	is not of even length the second list will be the longer.
%%
%%	List: The list to be split in half
%%		[Term()]
%%
split_in_half(List) ->
	split_in_half([],List,length(List) div 2).

split_in_half(First,Second,0) ->
	{lists:reverse(First),Second};

split_in_half(First,[Head|Tail],Length) ->
	split_in_half([Head|First],Tail,Length-1).

%%
%%	Creates a list of all pairs taken from the list provided
%%	NB: This is the super slow implementation
%%
all_pairs([]) -> [];

all_pairs([X|XS]) ->
    F = fun(Y) ->
          {X,Y}
    end,
    lists:map(F,XS) ++ all_pairs(XS).

%%
%%	Returns true if every element in List are equal (=:=), false otherwise.
%%	Empty lists and lists with one element are homogenous by definition.
%%	
%%	List: The list for checking
%%		[Term()]
%%
homogenous_list([]) -> true;

homogenous_list([_Elem|[]]) -> true;

homogenous_list([Elem1,Elem1|List]) ->
    homogenous_list(List);

homogenous_list(_List) ->
    false.

%%
%%	Returns the two lists combined with all duplicates removed.  This also has the effect of
%%	removing pre-existing duplicates in each of the lists as well as duplicates between the 
%%	two lists.
%%
%%	NB: This implementation is very slow.  Don't use for long lists.
%%
%%	List1: The first list
%%		[Term()]
%%	List2: The second list
%%		[Term()]
%%
combine_without_duplicates(List1,List2) ->
	combine_without_duplicates_int(List1++List2,[]).

combine_without_duplicates_int([],Result_List) ->
    Result_List;

combine_without_duplicates_int([Head|Tail],Result_List) ->
    case lists:member(Head,Result_List) of
        true
        	->	combine_without_duplicates_int(Tail,Result_List);
        false
        	->	combine_without_duplicates_int(Tail,[Head|Result_List])
    end.

%%
%%	Returns a list of every combination of one element from each list in Lists.
%%	
%%	It really seems like this could be parameterised to be take n elements from each list.
%%
%%	Lists: List of lists from which we will take a single element for our combinations
%%		[[Term()]]
%%
combinate([]) ->
    [[]];
    
combinate([List|Lists]) ->
	Combinations = combinate(Lists),
    push_all(List,Combinations).
    

%%
%%	Returns a list of lists, [[Term()]].  Each element in Heads will
%%	be appended to each list in Lists.  This will create a new list
%%	with the single additional element.
%%
%%	The number of lists returned = length(Heads) * length(Lists).
%%
%%	Heads: List of elements to attach to all the lists in Lists
%%		[Term()]
%%	Lists: List of lists to which 
%%
push_all(Heads,Lists) ->
    push_all(Heads,Lists,Lists).

push_all([],_Lists,_Complete_Lists) ->
    [];

push_all([_Head|Heads],[],Complete_Lists) ->
    push_all(Heads,Complete_Lists,Complete_Lists);

push_all(Heads,[List|Lists],Complete_Lists) ->
    [[hd(Heads)|List]|push_all(Heads,Lists,Complete_Lists)].
    
%%
%%	Returns the list provided minus it's last element.
%%	Empty lists are returned unmolested.  Single element
%%	lists will return an empty list.
%%
%%	List: The list whose last element will be dropped
%%		[Term()]
%%
drop_last([]) ->
    [];

drop_last([_Head|[]]) ->
    [];

drop_last([Head|Tail]) ->
    [Head|drop_last(Tail)].

%%
%%	Array functions
%%

%%
%%	Returns an array where the element indicated by Index is removed
%%	and all elements to the right of that element have been shifted left
%%	to fill the gap.
%%
%%	Index: The index of the element to be deleted
%%		Integer()
%%	Array: The array from which the element is to be deleted
%%		array()
%%
delete_element(Index,Array) ->
    Number_Of_Elements = array:size(Array),
    Index_List = lists:seq(Index,Number_Of_Elements),
    Move_Left = fun(Index_Arg,{Index_Arg,Array_Arg}) ->
                        case Index_Arg =:= Number_Of_Elements-1 of
                         	true
                                ->	{Number_Of_Elements,array:reset(Index_Arg,Array_Arg)};
                            false
                            	->	Left_Index = Index_Arg+1,
                         			Left_Element = array:get(Left_Index,Array_Arg),
                         			New_Array = array:set(Index_Arg,Left_Element,Array_Arg),
                         			{Left_Index,New_Array}
                        end
                end,
    Pred = fun({Index_Arg,_Array_Arg}) ->
                   Index_Arg /= Number_Of_Elements
           end,
    {_Used_Index,Final_Array} = util:foldl_abandon(Move_Left,{Index,Array},Index_List,Pred),
	array:resize(Number_Of_Elements-1,Final_Array).

%%
%%	Returns an array where the element indicated by Index is removed
%%	and all elements to the right of that element have been shifted left
%%	to fill the gap.
%%
%%	Indices: List of indices which are to be deleted from Array.
%%		[Integer()]
%%	Array: The array from which the element is to be deleted
%%		array()
%%
delete_elements(Indices,Array) ->
    Unique_Indices = sets:to_list(sets:from_list(Indices)),%% This is probably not the fastest way to remove all duplicate values
	Sorted_Indices = lists:sort(fun(A,B)-> A>B end,Unique_Indices),
	Delete_Element = fun(Index,Acc_Array) ->
                             delete_element(Index,Acc_Array)
                     end,
    lists:foldl(Delete_Element,Array,Sorted_Indices).

%%
%%	Returns an array where the element indicated by Index is removed
%%	and all elements to the right of that element have been shifted left
%%	to fill the gap.  This function requires that Indices be sorted from
%%	largest to smallest and that the list contains no duplicate values.
%%
%%	Indices: List of indices which are to be deleted from Array.  Must be
%%			 sorted from largest to smallest and contain no duplicate values.
%%		[Integer()]
%%	Array: The array from which the element is to be deleted
%%		array()
%%
delete_elements_unsafe(Indices,Array) ->
	Delete_Element = fun(Index,Acc_Array) ->
                             delete_element(Index,Acc_Array)
                     end,
    Return = lists:foldl(Delete_Element,Array,Indices),
    Return.

%%
%%	Returns a list of indices for the elements of Array.
%%
%%	Array: The array for which we want indices
%%		array()
%%
get_indices(Array) ->
    case array:size(Array) of
        0
        	->	[];
        Greater_Than_Zero
        	->	lists:seq(0,Greater_Than_Zero-1)
    end.

%%
%%	Returns a single list which is the result of adding the two lists provided together
%%	in the same manner as vector addition.  e.g. [1,2,3] + [4,5,6] = [5,7,9]
%%
%%		List1: List of numbers for adding
%%			[Number()]
%%		List2: List of numbers for adding
%%			[Number()]
%%
add_list_elements([],[]) ->
	[];

add_list_elements([H1|List1],[H2|List2]) ->
	[H1+H2|add_list_elements(List1,List2)].

%%
%%	IO type functions
%%

%%
%%	Returns the Pid of a file-handling process for a newly created file.
%%	Creates a file on the file-path provided using Tree_Metric and Identifier 
%%	to generate the file name.
%%
%%	Tree_Metric: Used to generate the file name.
%%		#tree_metric
%%	Identifier: An additional string which will be added to the file name
%%		String()
%%	File_Path: The file path defining the location at which the file will be created
%%		String()
%%
make_file(File_Name,File_Path) ->
	case filelib:is_dir(File_Path) of
		true
			->	file:open(File_Path ++ "/" ++ File_Name,[write]);
		false
			->	filelib:ensure_dir(File_Path),
				file:make_dir(File_Path),
				file:open(File_Path ++ "/" ++ File_Name,[write])
	end.

%%
%%	Date and time related functions
%%


%%
%%	Returns a String() time stamp which is formatted in the sensible NZ format.
%%	"dd/mm/yyyy hh:mm:ss"
%%
get_time_stamp() ->
    {Year,Month,Day} = erlang:date(),
    {Hour,Minute,Second} = erlang:time(),
    S_Year = util:int_to_string(Year),
    S_Month = util:int_to_string(Month),
    S_Day = util:int_to_string(Day),
    S_Hour = util:int_to_string(Hour),
    S_Minute = util:int_to_string(Minute),
    S_Second = util:int_to_string(Second),
	S_Day ++ "/" ++ S_Month ++ "/" ++ S_Year ++ " " ++ S_Hour ++ ":" ++ S_Minute ++ ":" ++ S_Second.

%%
%%	Returns a String() time stamp which can conveniently be used as a directory or file name.
%%
get_filename_time_stamp() ->
    {Year,Month,Day} = erlang:date(),
    {Hour,Minute,Second} = erlang:time(),
    S_Year = util:int_to_string(Year),
    S_Month = util:int_to_string(Month),
    S_Day = util:int_to_string(Day),
    S_Hour = util:int_to_string(Hour),
    S_Minute = util:int_to_string(Minute),
    S_Second = util:int_to_string(Second),
    S_Year ++ "-" ++ S_Month ++ "-" ++ S_Day ++ "_" ++ S_Hour ++ "-" ++ S_Minute ++ "-" ++ S_Second.

%%
%%	List type function
%%

%%
%%	Returns a new list, as for lists:map, applying Fun to each element of the list provided
%%	except that it only maps elements where pred(E) returns true for that element before the
%%	mapping function has been applied.
%%
filter_map(_Pred,_Fun,[]) -> [];

filter_map(Pred,Fun,[Head|Tail]) ->
	case Pred(Head) of
		true
			-> [Fun(Head)|filter_map(Pred,Fun,Tail)];
		_Anything_Else
			-> filter_map(Pred,Fun,Tail)
	end.

%%
%%	Returns a new list, as for lists:map, applying Fun to each element of the list provided except
%%	that it only maps elements where pred(E) returns true for that element after the mapping 
%%	function has been applied.
%%
map_filter(_Fun,_Pred,[]) -> [];

map_filter(Fun,Pred,[Head|Tail]) ->
	New_Head = Fun(Head),
	case Pred(New_Head) of
		true
			-> [New_Head|map_filter(Fun,Pred,Tail)];
		_Anything_Else
			-> map_filter(Fun,Pred,Tail)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%						Control structure functions							%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	Returns a list constructed from successive calls to Fun
%%
%%		Fun, function whose return values make up the new list
%%			fun()
%%		Size, the number of elements in the new list
%%			Integer
%%
generate_list(Fun,Size) when Size > 0 ->
    generate_list(Fun,Size,[]).

generate_list(_Fun,0,List) ->
    List;

generate_list(Fun,Size,List) ->
    generate_list(Fun,Size-1,[Fun()|List]).

%%
%%	Returns a list constructed from successive calls to Fun
%%	where Fun is expected to take a single argument which is
%%	current index (starting from 1) of this element in the list.
%%
%%		Fun: Function whose return values make up the new list
%%			fun(Num)
%%		Size: The number of elements in the new list
%%			Integer
%%
generate_numbered_list(Fun,Size) when Size > 0 ->
    generate_numbered_list(Fun,Size,[]).

generate_numbered_list(_Fun,0,List) ->
    List;

generate_numbered_list(Fun,Size,List) ->
    generate_numbered_list(Fun,Size-1,[Fun(Size)|List]).

%%
%%	Executes the provided function N times.
%%
%%	Fun: A parameterless function to be executed n times
%%		fun()
%%	N: The number of times Fun will be executed
%%		Integer()
%%
do_n_times(_Fun,N) when N =< 0 ->
	ok;

do_n_times(Fun,N) ->
    Fun(),
    do_n_times(Fun,N-1).

%%
%%	Returns a single value behaving exactly like lists:foldl except
%%	that computation is abandoned and the current accumulator is returned directly if 
%%	the predicate function, Pred, ever returns false.
%%
%%		Fun: The function which must take the Acc and the head of List and return a new Acc
%%			fun(A,B)->B
%%		Acc: The running accumulator
%%			term()
%%		List: The list providing arguments to Fun
%%			[term()]
%%		Pred: A function which takes the result of Fun/2, if true is returned we continue to
%%			  fold if false is returned folding is abandoned and the last result of Fun/2 is
%%			  returned.
%%			fun(B)->true|false
%%
foldl_abandon(_Fun,Acc,[],_Pred) -> Acc;

foldl_abandon(Fun,Acc,[Head|Body],Pred) ->
	Acc1 = Fun(Head,Acc),
    case Pred(Acc1) of
        true
        	->	foldl_abandon(Fun,Acc1,Body,Pred);
        false
        	->	Acc1
    end.

%%
%%	Returns the final application of Fun to Args.
%%	This function requires a function which takes a single argument which is
%%	of the same type as its return type.  In each function call Fun is applied
%%	to Args and the result becomes the Args passed into a recursive call to 
%%	loop_n_times while decementing the counter.  When the counter reaches 0
%%	Args is returned.
%%
%%	Fun: A function taking a single argument whose type is the same as Fun's return type
%%		fun()/1
%%	Args: The arguments to be passed into Fun
%%		Term()
%%	N: The number of times that Fun should be called with successive Arg values
%%		Integer()
%%
loop_n_times(_Fun,Args,0) ->
    Args;
    
loop_n_times(Fun,Args,N) ->
    New_Args = Fun(Args),
    loop_n_times(Fun,New_Args,N-1).
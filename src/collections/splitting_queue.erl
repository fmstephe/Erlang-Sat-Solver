%% Author: Francis Stephens
%% Created: 8 Oct 2008
%% Description: TODO: Add description to splitting_queue
-module(splitting_queue).

%%
%% Include files
%%

-include("collection_records.hrl").

%%
%% Exported Functions
%%
-export([new/0,is_empty/1,enqueue/2,dequeue/1,peak/1,split_in_half/1,split_alternating/1]).

-compile(export_all).

%%
%% API Functions
%%

%%%
%%%	A #splitting_queue is divided into two lists the front and rear lists.
%%%	Elements are enqueued onto the rear list and dequeued off the front
%%%	list by merely taking the head of that list.
%%%	There is a structural invariant where the front must never be empty while
%%%	the rear list contains elements.  Where this occurs during some operation on
%%%	the queue the rear list is reversed and becomes the front list while new rear
%%%	list is made empty.
%%%
-record(splitting_queue,{front,rear}).


%%
%%	Returns an empty #splitting_queue
%%
new() ->
	#splitting_queue{front=[],rear=[]}.

%%
%%	Returns true if a call to deqeue/1 with the #splitting_queue  provided
%%	would return true, false otherwise.
%%
%%	Splitting_Queue: The #splitting_queue to be tested for emptiness
%%		#splitting_queue
%%
is_empty({splitting_queue,[],[]}) ->
    true;

is_empty(_Splitting_Queue) ->
    false.

%%
%%	If the #splitting_queue provided is not empty this function returns a 
%%	#dequeue containing the element at the front of this queue and a
%%	#splitting_queue which contains the same elements in the same order
%%	as the queue provided except with the front element removed.  If the
%%	#splitting_queue provided is empty then the atom empty is returned.
%%
%%	Splitting_Queue: The #splitting_queue from which to dequeue the front element
%%		#splitting_queue
%%
dequeue({splitting_queue,[],[]}) ->
    empty;

dequeue({splitting_queue,[Head|Tail],Rear}) -> 
	New_Queue = check_front(Tail,Rear),
    #dequeue{element=Head,splitting_queue=New_Queue}.

%%
%%	If the #splitting_queue provided is not empty this function returns a 
%%	#peak containing that would be returned by the function #dequeue
%%	but does not return a new #splitting_queue as per dequeue/1.  If
%%	the queue provided is empty then the atom empty is returned.
%%
%%	Splitting_Queue: The #splitting_queue whose front element we want to peak at
%%		#splitting_queue
%%
peak({splitting_queue,[],[]}) -> 
	empty;

peak({splitting_queue,[Head|_Tail],_Rear}) -> 
	#peak{element=Head}.

%%
%%	Returns a #splitting_queue with the element provided added to the end
%%	of the queue.
%%
%%	Element: The element to add to the end of the #splitting_queue provided
%%		Term()
%%	Splitting_Queue: The #splitting_queue to which Element will be added
%%		#splitting_queue
%%
enqueue(Element,{splitting_queue,Front,Rear}) -> 
	check_front(Front,[Element|Rear]).

%%
%%	Returns a #split containing two #splitting_queues where the first half of
%%	of the queue is in one and the second half is in another.
%%
%%	Splitting_Queue: The #splitting_queue to be split in half
%%		#splitting_queue
%%
split_in_half({splitting_queue,Front,Rear}) -> 
	Rev_Rear = lists:reverse(Rear),
	{Front1,Front2} = combine_and_split(Front,Rev_Rear),
	Queue1 = #splitting_queue{front=Front1,rear=[]},
	Queue2 = #splitting_queue{front=Front2,rear=[]},
	#split{splitting_queue_1=Queue1,splitting_queue_2=Queue2}.


%%**************************************%%
%%	Local functions for split in half	%%
%%**************************************%%

%%
%%	Returns a tuple containing two lists which is the same as if Last
%%	was appended onto First and then the two lists were split in half
%%	into two equal length lists.  If the total length of First and Last
%%	is uneven it is not defined which of the two lists returned will be
%%	longer.
%%
%%	First: The first list
%%		[Term()]
%%	Last: The last list
%%		[Term()]
%%
combine_and_split(First,Last) ->
	First_Length = length(First),
	Last_Length = length(Last),
	Difference = abs(First_Length - Last_Length) =< 1,
	case Difference of
		true
			->	{First,Last};
		false
			->	combine_and_split([],First,Last,(First_Length+Last_Length) div 2)
	end.

combine_and_split(Acc,First,Last,0) ->
	{lists:reverse(Acc),First++Last};

combine_and_split(Acc,[],[Head|Tail],Length) ->
	combine_and_split([Head|Acc],[],Tail,Length-1);

combine_and_split(Acc,[Head|Tail],Last,Length) ->
	combine_and_split([Head|Acc],Tail,Last,Length-1).

%%
%%	Returns a #split containing two #splitting_queues where the elements in the
%%	#splitting_queue provided are divided into two sets by dequeuing the elements
%%	and alternating between enqueueing into each of the two new #splitting_queues.
%%
%%	Splitting_Queue: The #splitting_queue to be split in half
%%		#splitting_queue
%%
split_alternating({splitting_queue,Front,Rear}) ->
    Rev_Rear = lists:revers(Rear),
	split_alt([],[],true,Front,Rev_Rear).

split_alt(First,Second,true,[Head|Tail],Rear) ->
	split_alt([Head|First],Second,false,Tail,Rear);

split_alt(First,Second,false,[Head|Tail],Rear) ->
	split_alt(First,[Head|Second],true,Tail,Rear);

split_alt(First,Second,true,[],[Head|Tail]) ->
	split_alt([Head|First],Second,false,[],Tail);

split_alt(First,Second,false,[],[Head|Tail]) ->
	split_alt(First,[Head|Second],true,[],Tail);

split_alt(First,Second,_Ignored,[],[]) ->
	{lists:reverse(First),lists:reverse(Second)}.

%%
%% Local Functions
%%

%%
%%	Returns a #splitting_queue which contains the same elements in the same
%%	order as the one provided except where if the one provided had an empty
%%	front list the rear list is reversed and becomes the new front.
%%
%%	Front: The front list to be checked
%%		[Term()]
%%	Rear: The rear list to be checked
%%		[Term()]
%%
check_front([],Rear) ->
    #splitting_queue{front=lists:reverse(Rear),rear=[]};

check_front(Front,Rear) ->
    #splitting_queue{front=Front,rear=Rear}.
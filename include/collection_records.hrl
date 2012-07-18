%% Author: Francis Stephens
%% Created: 1 Dec 2007
%% Description: Erlang Header file defining useful common records for communicating sat solver modules

%%
%% Records
%%

-record(dequeue,{element,splitting_queue}).

-record(split,{splitting_queue_1,splitting_queue_2}).

-record(peak,{element}).
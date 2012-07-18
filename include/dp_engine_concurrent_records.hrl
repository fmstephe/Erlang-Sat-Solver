%% Author: Francis Stephens
%% Created: 1 Dec 2007
%% Description: Erlang Header file defining useful common records for communicating sat solver modules

%%
%% Records
%%

%%%
%%%	A #engine_concurrent_args contains the additional arguments required by the
%%%	dp_engine_concurrent engine.
%%%
%%%	network_config: The configuration parameters for the process_tree used by the engine
%%%		#network_config
%%%	comm_freq: The frequency (number of solver steps) between each network communication
%%%					Having a high number means less network communication overhead and hopefully more work done.
%%%					However, a high number means that network messages take considerably longer to propogate and
%%%					network processes without work will starve for longer.  A low number produces more network
%%%					communication overhead but allows network messages to propogate faster.  Clearly a tradeoff
%%%					lies here.
%%%		Integer()
%%%	process_tree_mod: The process tree module used by the solver engine to manage concurrent solving.
%%%		atom()
%%%
-record(engine_concurrent_args,{network_config,comm_freq,process_tree_mod}).
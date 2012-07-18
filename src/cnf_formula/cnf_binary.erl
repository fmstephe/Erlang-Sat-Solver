%% Author: Francis Stephens
%% Created: 5 Dec 2008
%% Description: TODO: Add description to cnf_binary
-module(cnf_binary).

%%
%%	Include files
%%

-include("sat_records.hrl").

%%
%%	Pseudo Records
%%	Below is a collection of definitions for the layour of a binary
%%	representation of CNF formulae
%%	NB: All Integer representations are big endian (the erlang default)
%%		All numbers are base 16 (<<16#12345678:32>>
%%

%%%
%%%	The number of variables in a formula is defined by the first 32 bits.
%%%	From here on we will denote the first 32 bits as N.
%%%
%%%	The next 32 bits states how many clauses there are in the formula.  All binary formula are in 3-sat
%%%	structure so we don't need to record the size of each clause individually.
%%%	From here on we will denote the second 32 bits as C.
%%%
%%%	Each variable is defined by three bits (having five legal values).
%%%	The size of this bit block is N*3+(((8-((N*3)%8)))%8).
%%%	The rather odd looking (8-((N*3)%8))%8 just means that if N*3 does not divide by 8 we have to tack on some 
%%%	bits to make sure we can divide the variable bits into 8 bit blocks.
%%%	We use three bits to represent the five possible states of a variable in the sat solver.
%%%		1: Never assigned 		(000)
%%%		2: True assigned once 	(110)
%%%		3: False assigned once	(010)
%%%		4: True assigned twice	(111)
%%%		5: False assigned twice	(011)
%%%	From here on we will denote this block of bits defining all of the variables as V.
%%%
%%%	Once we know how many variables there are we know the least number of bits required to assign a unique index to
%%%	each one, I.  I + 1 is the number of bits used to denote a literal, the last (right most) bit indicates
%%%	whether this literal is negated (0) or not (1).
%%%	I is defined as being the least power of two capable of expressing the value N.
%%%	-> 2^I >= N (express as some fancy log value I imagine)
%%%	Since every clause has 3 literals we can multiply by the number of clauses to see how many bits this block takes up.
%%%	Once again we may need to fill in some additional bits at the end.
%%%	((I+1)*C)+((8-(((I+1)*C)%8))%8).
%%%	From here on we will denote this block of bits as F.
%%%
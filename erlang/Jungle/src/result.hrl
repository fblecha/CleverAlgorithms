%% File: result.hrl

%%-----------------------------------------------------------
%% Data Type: generation
%% where:
%%    number:  the generation number
%%    top:   the top N finishers in this generation (ranked by fitness) in order
%%    average_fitness: the average fitness of the entire population of this generation
%%------------------------------------------------------------
-record(result, {number, top=[], average_fitness}).

%% @author Frank Blecha
%% @since 1/10/2008
%% @version 0.1

-module(result).

%%-----------------------------------------------------------
%% Data Type: generation
%% where:
%%
%%    number:  the generation number
%%
%%    top: the top N finishers in this generation (ranked by fitness)
%%    in order
%%
%%    average_fitness: the average fitness of the entire population of
%%    this generation
%%
%%    %%------------------------------------------------------------
-record(result, {number, top=[], average_fitness}).

-module(experiment).
-export([experiment_name/1, number_of_generations/1, population_size/1, percent_kept_per_generation/1, per_bit_mutation_rate/1,print_results/1 ]).

-import(io).

experiment_name(ExperimentParameters) ->
    {experiment, {name,Name}, {_,_}, {_,_}, {_,_}, {_,_}} = ExperimentParameters,
    Name.

number_of_generations(ExperimentParameters) ->
    {experiment, {_,_}, {generations,Generations}, {_,_}, {_,_}, {_,_}} = ExperimentParameters,
    Generations.

population_size(ExperimentParameters) ->
    {experiment, {_,_}, {_,_}, {population_size, Population_Size}, {_,_}, {_,_}} = ExperimentParameters,
    Population_Size.

percent_kept_per_generation(ExperimentParameters) ->
    {experiment, {_,_}, {_,_}, {_,_}, {percent_kept_per_generation,Percent_Kept_Per_Generation}, {_,_}} = ExperimentParameters,
    Percent_Kept_Per_Generation.

per_bit_mutation_rate(ExperimentParameters) ->
    {experiment, {_,_}, {_,_}, {_,_}, {_,_}, {per_bit_mutation_rate,Per_Bit_Mutation_Rate}} = ExperimentParameters,
    Per_Bit_Mutation_Rate.



print_results(Results) ->    
    io:fwrite("Results = ~p~n",[Results]).



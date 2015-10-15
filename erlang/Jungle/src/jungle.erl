%% @author Frank Blecha
%% @since 1/10/2008
%% @version 0.1

-module(jungle).

-export([test/0, profile/0]).
-import(random).
-import(lists).
-import(io).
-import(bin_util).
-import(barn).
-import(stat).
-import(experiment).
-import(fprof).
-import(result).

%% @type 
-record(result, {number, top=[], average_fitness}).

%% @type chromosomes() = {chromosomes, binary()}
%% @type fitness() = {fitness, float()}
%% @type individual() = {individual, chromosomes(), fitness()}



%% Run the test and profile the results using fprof
profile() ->
    fprof:apply(jungle, test,[]),
    fprof:profile(),
    fprof:analyse().


%% Run a test of the Jungle GA system.
test() ->
    %% Set the RNG seeds
    random:seed(1,2,3),


    ExperimentParameters = {experiment, 
			    {name, "test"},
			    {generations, 50},
			    {population_size, 500},
			    {percent_kept_per_generation, 0.5},
			    {per_bit_mutation_rate, 0.00001}},

    Initial_Population = create_initial_population(ExperimentParameters),

    Results = experiment(ExperimentParameters, Initial_Population),

    experiment:print_results(Results).


%% Create the initial population for the experiment
%%
%% @todo Should pass in the entity creation function
%%
create_initial_population(ExperimentParameters) ->
    Default_Individual_Creator = fun() -> create_individual(11) end,
    Initial_Population = create_random_list(experiment:population_size(ExperimentParameters), Default_Individual_Creator, []),
    %io:fwrite("Initial Population = ~p~n",[length(Initial_Population)]),  
    Initial_Population.

%% Run the given experiment with the given population
%% 
experiment(ExperimentParameters, Initial_Population) ->
    evaluate_generation(experiment:number_of_generations(ExperimentParameters),ExperimentParameters, Initial_Population,[]).

%% This actually iterates the experiment using the given population and the generation number. 
%%
%% @spec evaluate_generation(X::integer(), Y::List(), Z::List()) -> List
%% where
%%      X is the current generation number
%%      Y is the list containing the individuals in the current population
%%      Z is the list of the results for each generation
evaluate_generation(0, _, Population, Results) -> 
    Population,
    Results;
evaluate_generation(NumberOfGenerationsLeft, ExperimentParameters, Population, Results) ->
    Generation = experiment:number_of_generations(ExperimentParameters) - NumberOfGenerationsLeft,

    io:fwrite("Evaluating Generation = ~p~n",[Generation]),

    RankedPopulation = measure_population_fitness(Population,<<"Frank Rocks">>),
    Partition = round(experiment:population_size(ExperimentParameters) * experiment:percent_kept_per_generation(ExperimentParameters) ),
    {Winners,Losers} = lists:split(Partition, RankedPopulation),

    %%Let's figure out the average fitness and see how we're doing
    Average_Fitness = average_fitness(RankedPopulation),

    %%assuming there's at least 3 winners XXX
    Top = lists:sublist(Winners,3),
    Result = #result{number=Generation, top=Top, average_fitness=Average_Fitness},
    

    NextPopulation = make_next_generation(Winners, ExperimentParameters),

    NextResults = Results ++ [Result],
    evaluate_generation(NumberOfGenerationsLeft - 1, ExperimentParameters, NextPopulation, NextResults).

%%
%% Calcuate the average fitness of the given population
%%
average_fitness(Population) ->
    Fitness = fun(Elem) -> get_individual_fitness(Elem) end,
    stat:avg(Population, Fitness).

%% Mate the given parents using crossover; return both OffspringA and
%% OffspringB as a tuple
%%
mate({ParentA, ParentB}) ->
    {OffspringA, OffspringB} = crossover(ParentA,ParentB),
    %%[ParentA, ParentB, OffspringA, OffspringB].
    [OffspringA, OffspringB].

%% Make a child from the given population.  This implementation chooses
%% one parent randomly (ParentA) and mates it with another randomly
%% chosen parent (ParnetB).  Fitness is not used to help determine
%% which mating pairs are created (in this implementation).  Of the
%% two offspring that are produced, one is chosen randomly to be "the"
%% offspring and is returned.
%%
%% @see jungle::crossover
%%
make_child(ExperimentParameters, Population) ->
    Population_Size = length(Population),

    ParentA = lists:nth(random:uniform(Population_Size) , Population),
    ParentB = lists:nth(random:uniform(Population_Size) , Population),
    
    [ChildA, ChildB ] = mate( {ParentA, ParentB} ),
    OffSpringBeforeMutation = lists:nth(1,barn:random_select( [ChildA, ChildB], 1)),
    OffSpring = mutate(ExperimentParameters, OffSpringBeforeMutation ),
    OffSpring.


%% From the given population, make N children from From to To
%% (e.g. From = 1, To = 10)
make_N_children(ExperimentParameters, Population,From,To) when From < To ->
    [ make_child(ExperimentParameters, Population) | make_N_children(ExperimentParameters, Population,From+1,To)];
make_N_children(ExperimentParameters, Population,From,From) ->
    [make_child(ExperimentParameters, Population)].


%% make the next generation using the given experiement parameters and
%% the initial population.  Return the List that should comprise the
%% next generation.
%%
%% @spec make_next_generation(Initial_Population::List(), ExperimentParameters::result()) -> List
%%
%% 
make_next_generation(Initial_Population, ExperimentParameters) ->
    NumberOfOffspringToProduce = experiment:population_size(ExperimentParameters) - length(Initial_Population),    
    Children = make_N_children(ExperimentParameters, Initial_Population, 1, NumberOfOffspringToProduce ),
    lists:flatten( [ Initial_Population, Children ] ).

%% Measures the fitness of the population compared to the given goal.
%% Each individual is sorted by their fitness in the resulting list.
measure_population_fitness(Population, Goal) ->
    Cost = fun(Individual) -> evaluate_individual(Individual,Goal) end,
    SortByMaxFitness = fun(A,B) -> sort_by_max_fitness(A,B) end,
    lists:sort(SortByMaxFitness, lists:map(Cost,Population)).

%%this should really be assign new fitness XXX    
create_new_individual(Individual,NewFitness) ->
    {_, {_,Chromosomes},{_,_}} = Individual,
    {individual,{chromosomes,Chromosomes},{fitness,NewFitness}}.

get_individual_fitness(Individual) ->
    {_, {_,_},{_,Fitness}} = Individual,
    Fitness.

%% Used as a comparator for sorting a list of individuals.  Will
%% return a boolean depending on if IndividualA.fitness >
%% IndividualB.fitness
sort_by_max_fitness(IndividualA,IndividualB) ->
    {_, {_,_},{_,FitnessA}} = IndividualA,
    {_, {_,_},{_,FitnessB}} = IndividualB,
    FitnessA > FitnessB.


%% Create a list of random "stuff" using the given Fun as a generator
create_random_list(N,Fun,[]) -> 
	create_random_list(N-1, Fun, [Fun()]);
create_random_list(N, Fun, List) ->
    case N of 
        0 -> lists:reverse(List);%%done
        _ -> create_random_list(N-1, Fun,[ Fun() | List ]) 
	end.

%% Create a new individual using the given number of chromosomes.  In
%% this case, one chromosome maps to one integer, and will be
%% evaluated as such by the fitness function.
create_individual(NumberOfChromosomes) -> 	
    Create = fun() -> random:uniform(200) end, %%XXX
    Individual = {individual, 
                  {chromosomes, list_to_binary(create_random_list(NumberOfChromosomes,Create,[]))},
                  {fitness,-1000000}
                  },
    Individual.

%% Evaluate the individual in reference to the given Goal.  Create a
%% new individual using the given number of chromosomes.  In this
%% case, one chromosome maps to one integer, and will be evaluated as
%% such by the fitness function.
evaluate_individual(Individual, Goal) ->
    {_, {_,Chromosomes},{_,_}} = Individual,


    NumberBytes = size(Chromosomes),
    NumberBits = NumberBytes * 8,

    <<A:NumberBits>> = Chromosomes,
    <<B:NumberBits>> = Goal,

    HammingDistance = bin_util:bit_count( A bxor B ),
    Fitness = HammingDistance * -1,
    NewIndividual = create_new_individual(Individual,Fitness),
    NewIndividual.




%% Perform crossover for the given parents.  A random bit position
%% will be chosen, and then the bit stream from ParentA will be
%% crossed with the bitstream for ParentB at that point.  Two
%% offspring will be produced (ParentA/ParentB and ParentB/ParentA).
%%
%%
%% @spec crossover(individual(),individual()) -> {individual(),individual()}
%%
%%
crossover(ParentA,ParentB) ->
    %%it doesn't matter which parent is chosen here, they'll both have
    %%the same # of chromosomes
    {_, {_,Chromosomes},{_,_}} = ParentA,

    NumberBytes = size(Chromosomes),
    NumberBits = NumberBytes * 8,

    Location = random:uniform(NumberBytes),

    BitLocation = Location * 8,
    %%io:fwrite("Location = ~p~n",[Location]),
    %%io:fwrite("BitLocation = ~p~n",[BitLocation]),

    {_, {_,ParentA_Chromosomes},{_,_}} = ParentA,
    {_, {_,ParentB_Chromosomes},{_,_}} = ParentB,

    LeftSize = BitLocation,
    RightSize = NumberBits - LeftSize,
    <<LeftSideA:LeftSize, RightSideA:RightSize>> = ParentA_Chromosomes,
    <<LeftSideB:LeftSize, RightSideB:RightSize>> = ParentB_Chromosomes,
    AB_Chromosomes = <<LeftSideA:LeftSize, RightSideB:RightSize>>,
    BA_Chromosomes = <<LeftSideB:LeftSize, RightSideA:RightSize>>,    

    OffspringAB = {individual,{chromosomes,AB_Chromosomes},{fitness,-10000}},
    OffspringBA = {individual,{chromosomes,BA_Chromosomes},{fitness,-10000}},

    %%io:fwrite("A = ~p~n",[ParentA]),
    %%io:fwrite("B = ~p~n",[ParentB]),
    %%io:fwrite("AB = ~p~n",[OffspringAB]),
    %%io:fwrite("BA = ~p~n",[OffspringBA]),

    {OffspringAB, OffspringBA}.

%% Mutate the given individual's bit stream using the mutation rate
%% given in the experiment parameters.  Please note that for this
%% implementation, I'm using 8 bit integers to encode for a gene.
%% While the per bit mutation rate might be something like 0.1, I'm
%% actually using the mutation rate as 8 (bits) X mutation rate
%% (e.g. 8 x 0.1 = 0.8) to account for mutation occuring in the gene.
%% If mutation occurs (in this implementation), I just generate a new
%% integer randomly.
%%
%% @todo look into using bit stream comprehensions to flip these bits
%% individually 
%%
%% @todo audit when mutations occur
%%
mutate(ExperimentParameters, Individual) ->
    %%for now, I'm going to mutate 8bit integers, so I'll do MutationRate * 8 
    PerBit = experiment:per_bit_mutation_rate(ExperimentParameters),
    Mutate = fun(X) ->
		     RandomPull = random:uniform(),
		     
		     if
			 RandomPull =< (PerBit * 8)  ->
			     %io:fwrite("***** mutation occured ******* ~n",[]),
			     Result = random:uniform(255);
			 true  ->
			     Result = X
		     end,
		     Result
	     end,
    {_, {_,Chromosomes},{_,_}} = Individual,
    

    NewChromosomes = << << (Mutate(X)) >> || <<X>> <= Chromosomes >>,
    NewIndividual = {individual,{chromosomes,NewChromosomes},{fitness,-10000}},
    NewIndividual.



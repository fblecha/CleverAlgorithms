import random

#Python impl of http://www.cleveralgorithms.com/nature-inspired/stochastic/hill_climbing_search.html

def pretty_print_vector(vector):
    return "".join(map(str,vector))

def objective_function(vector):
    ''' max value is all ones'''
    return pretty_print_vector(vector).count("1")

onemax = objective_function


def random_bitstring(num_bits):
    return [ 1 if random.random() < 0.5 else 0 for i in range(0, num_bits)]

def random_neighbor(bitstring):
    mutant = list(bitstring)
    pos = random.randint(0, len(bitstring)-1)
    mutant[pos] = 0 if mutant[pos]==1 else 1  # flip the bit
    return mutant

def search(max_iterations, num_bits):
    all_candidates = []
    candidate = {}
    candidate['vector'] = random_bitstring(num_bits)
    candidate['cost'] = onemax(candidate['vector'])
    for i in range(0, max_iterations):
        neighbor = {}
        neighbor['vector'] = random_neighbor(candidate['vector'])
        neighbor['cost'] = onemax(neighbor['vector'])
        candidate = neighbor if neighbor['cost'] >= candidate['cost'] else candidate
        all_candidates.append( (i, candidate['vector'], candidate['cost']) )
       
        if candidate['cost'] == num_bits:
            break
                                  
    return candidate, all_candidates

# I'm not bothering to wrap this in a nice Usage/main pattern...

if __name__ == "__main__":

    #random.seed(0)#make it repeatable

    num_bits = 4
    max_iter = 1000
    best, all_candidates = search(max_iter, num_bits)

    for iteration, vector, cost in all_candidates[::100]:
        print "> iteration # %s, best = #%s with score = %s" % (iteration, pretty_print_vector(vector), cost)

    print "Done.  Best solution: score=%s, v=%s" % (best['cost'], pretty_print_vector(best['vector']))


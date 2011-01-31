import random
#Python impl of http://www.cleveralgorithms.com/nature-inspired/stochastic/random_search.html

def objective_function(vector):
    return reduce(lambda a, b:a + b**2, vector)

def random_vector(minmax):
    return [minmax[i][0] + ((minmax[i][1] - minmax[i][0]) * random.random()) 
                            for i in range(0,len(minmax)) ]

def search(search_space, max_iter):
    best = None
    for i in range(1, max_iter):
        candidate = {}
        candidate['vector'] = random_vector(search_space)
        candidate['cost'] = objective_function(candidate['vector'])
        if best == None or candidate['cost'] < best['cost']:
            best = candidate

    return best


# I'm not bothering to wrap this in a nice Usage/main pattern...

if __name__ == "__main__":

    random.seed(0)#make it repeatable

    problem_size = 5
    search_space = [ [-5,5] for i in range(0,problem_size)]
    max_iter = 10
    best = search(search_space, max_iter)
    print "Done.  Best solution: c=%s, v=%s" % (best['cost'], best['vector'])


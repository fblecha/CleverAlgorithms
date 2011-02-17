import random
#Python impl of http://www.cleveralgorithms.com/nature-inspired/stochastic/adaptive_random_search.html


def objective_function(vector):
    return reduce(lambda a, b:a + b**2, vector)

def random_vector(minmax):
    return [rand_in_bounds(minmax[i][0], minmax[i][1]) 
                           for i in range(0,len(minmax)) ]
            
def rand_in_bounds(min_num,max_num):
    return min_num + ((max_num - min_num) * random.random())


def take_step(minmax, current, step_size):
    position = []
    for i in range(0, len(current)):
        a_min = max(minmax[i][0], current[i] - step_size)
        a_max = min(minmax[i][1], current[i] + step_size)
        position.insert( i, rand_in_bounds(a_min, a_max) )
    return position


def large_step_size(my_iter, step_size, s_factor, l_factor, iter_mult):
    factor = l_factor if (my_iter > 0 and ((my_iter % iter_mult)==0)) else s_factor
    return step_size * factor

    

def take_steps(bounds, current,step_size, big_stepsize):
    step = {}
    big_step = {}

    for a, s in [(step, step_size), (big_step, big_stepsize)]:
        a['vector'] = take_step(bounds, current['vector'], s)
        a['cost'] = objective_function(a['vector'] )

    return step, big_step


def search(max_iter, bounds, init_factor, s_factor, l_factor, iter_mult, max_no_impr):
    step_size = (bounds[0][1] - bounds[0][0]) * init_factor
    current = {}
    count = 0
    current['vector'] = random_vector(bounds)
    current['cost'] = objective_function(current['vector'])
    for my_iter in range(0, max_iter):
        big_stepsize = large_step_size(my_iter, step_size, s_factor, l_factor, iter_mult)
        step, big_step = take_steps(bounds, current, step_size, big_stepsize)
        if step['cost'] <= current['cost'] or big_step['cost'] <= current['cost']:
            if big_step['cost'] <= step['cost']:
                step_size, current = big_stepsize, big_step
            else:
                current = step
            count = 0
        else:
            count += 1
            if count >= max_no_impr:
                count = 0
                step_size = step_size/s_factor
        print " > iteration #%s, best = #%s" % (my_iter+1, current['cost'])
    return current
    

# I'm not bothering to wrap this in a nice Usage/main pattern...

if __name__ == "__main__":

    random.seed(0)#make it repeatable

    problem_size = 2
    bounds = [ [-5,5] for i in range(0,problem_size)]
    max_iter = 1000
    init_factor = 0.05
    s_factor = 1.3
    l_factor = 3.0
    iter_mult = 10
    max_no_impr = 30
    best = search(max_iter, bounds, init_factor, s_factor, l_factor, iter_mult, max_no_impr)
    print "Done.  Best solution: c=%s, v=%s" % (best['cost'], best['vector'])


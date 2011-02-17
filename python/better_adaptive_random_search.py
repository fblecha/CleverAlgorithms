import random
import sys

#see adaptive_random_search.py, the better here stands for: (1)readable and (2)less code

def rand_in_bounds(min_num,max_num):
    return min_num + ((max_num - min_num) * random.random())

def take_step(minmax, current_solution, step_size):
    position = []

    for i,value in enumerate(current_solution.vector):
        a_min = max(minmax[i][0], value - step_size)
        a_max = min(minmax[i][1], value + step_size)
        position.insert( i, rand_in_bounds(a_min, a_max) )
    return position


def large_step_size(my_iter, step_size, s_factor, l_factor, iter_mult):
    factor = l_factor if (my_iter > 0 and ((my_iter % iter_mult)==0)) else s_factor
    return step_size * factor


def take_steps(bounds, current_solution,step_size, big_stepsize):
    step = Solution()
    big_step = Solution()

    for a, s in [(step, step_size), (big_step, big_stepsize)]:
        a.vector = take_step(bounds, current_solution, s) 

    return step, big_step


class Solution:
    
    def __init__(self, bounds=None):
        if bounds:
            self.vector = [rand_in_bounds(bounds[i][0], bounds[i][1]) 
                           for i in range(0,len(bounds)) ]

    def get_vector(self):
        return self.vector
    def set_vector(self, v):
        self.vector = v

    vector = property(get_vector, set_vector)

    def cost(self):
        #if the array isn't setup, set the cost to -maxint
        return reduce(lambda a, b:a + b**2, self.vector) if self.vector else -sys.maxint

    def __str__(self):
        return "cost=%s, vector=%s" % (self.cost(), self.vector )


def search(max_iter, bounds, init_factor, s_factor, l_factor, iter_mult, max_no_impr):
    step_size = (bounds[0][1] - bounds[0][0]) * init_factor
    count = 0
    current = Solution(bounds)

    for my_iter in range(0, max_iter):
        big_stepsize = large_step_size(my_iter, step_size, s_factor, l_factor, iter_mult)
        step, big_step = take_steps(bounds, current, step_size, big_stepsize)
 
        if step.cost() <= current.cost() or big_step.cost() <= current.cost():
            if big_step.cost() <= step.cost():
                step_size, current = big_stepsize, big_step
            else:
                current = step
            count = 0
        else:
            count += 1
            if count >= max_no_impr:
                count = 0
                step_size = step_size/s_factor
        print " > iteration #%s, best = #%s" % (my_iter+1, current.cost() )
    return current
    

# I'm not bothering to wrap this in a nice Usage/main pattern...

if __name__ == "__main__":

    random.seed(0)#make it repeatable

    problem_size = 2
    bounds = [[-5,5]] * problem_size
    max_iter = 1000
    init_factor = 0.05
    s_factor = 1.3
    l_factor = 3.0
    iter_mult = 10
    max_no_impr = 30
    best = search(max_iter, bounds, init_factor, s_factor, l_factor, iter_mult, max_no_impr)
    print "Done.  Best solution: %s" % (best)


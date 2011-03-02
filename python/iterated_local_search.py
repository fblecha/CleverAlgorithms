# implements http://www.cleveralgorithms.com/nature-inspired/stochastic/iterated_local_search.html

import math
import random

def euc_2d(city1,city2):
    return round( math.sqrt( (city1[0]-city2[0])**2 + (city1[1] - city2[1])**2) )

def cost(permutation, search_space):
    distance = 0
    for i, c1 in enumerate(permutation):
        c2 = permutation[0] if (i==len(permutation)-1) else permutation[i+1]
        distance += euc_2d(search_space[i], c2)
    return distance
    

def random_permutation(search_space):
    newList =  list(search_space) 
    random.shuffle( newList )
    return newList

def stochastic_two_opt(permutation):
    perm = list(permutation)

    c1 = random.randint(0, len(perm)-1)
    c2 = random.randint(0, len(perm)-1)
    exclude = [c1]
    if c1==0:
        exclude.append( len(perm) - 1 )
    else:
        exclude.append( c1 - 1 )
    if c1 == (len(perm)-1):
        exclude.append( 0 )
    else:
        exclude.append( c1 + 1 )
    while c2 in exclude:
        c2 = random.randint(0, len(perm)-1)
    if c2 < c1:
        c1,c2 = c2, c1
    tmp = perm[c1:c2]
    tmp.reverse()
    perm[c1:c2] = tmp
    return perm


def local_search(best, search_space, max_no_improv):
    count = 0
    while count < max_no_improv:
        candidate = {}
        candidate['vector'] = stochastic_two_opt(best['vector'])
        candidate['cost'] = cost(candidate['vector'], search_space)
        count = 0 if candidate['cost'] < best['cost'] else count + 1
        best = candidate if candidate['cost'] < best['cost'] else best
    return best

def double_bridge_move(perm):
    pos1 = 1 + random.randint(0, len(perm) / 4 )
    pos2 = pos1 + 1 + random.randint(0, len(perm) / 4)
    pos3 = pos2 + 1 + random.randint(0, len(perm) / 4)
    p1 = perm[0:pos1] + perm[pos3:len(perm)]
    p2 = perm[pos2:pos3] + perm[pos1:pos2]
    return p1 + p2

def perturbation(search_space, best):
    candidate = {}
    candidate['vector'] = double_bridge_move(best['vector'])
    candidate['cost'] = cost(candidate['vector'], search_space)
    return candidate


def search(search_space, max_iterations, max_no_improv):
    best = {}
    best['vector'] = random_permutation(search_space)
    best['cost'] = cost(best['vector'], search_space)
    best = local_search(best, search_space, max_no_improv)
    for i in range(0, max_iterations):
        candidate = perturbation(search_space, best)
        candidate = local_search(candidate, search_space, max_no_improv)
        best = candidate if candidate['cost'] < best['cost'] else best
        print "> iteration #%s, best=#%s" % (i+1, best['cost'])
    return best


# I'm not bothering to wrap this in a nice Usage/main pattern...

if __name__ == "__main__":
  
    
    #random.seed(0) #let's make this repeatable

    berlin52 = [[565,575],[25,185],[345,750],[945,685],[845,655],
                [880,660],[25,230],[525,1000],[580,1175],[650,1130],[1605,620],
                [1220,580],[1465,200],[1530,5],[845,680],[725,370],[145,665],
                [415,635],[510,875],[560,365],[300,465],[520,585],[480,415],
                [835,625],[975,580],[1215,245],[1320,315],[1250,400],[660,180],
                [410,250],[420,555],[575,665],[1150,1160],[700,580],[685,595],
                [685,610],[770,610],[795,645],[720,635],[760,650],[475,960],
                [95,260],[875,920],[700,500],[555,815],[830,485],[1170,65],
                [830,610],[605,625],[595,360],[1340,725],[1740,245]]
    max_iterations = 1000
    max_no_improv = 50
    best = search(berlin52, max_iterations, max_no_improv)
    print "Done. Best solution: c=#%s, v=%s" % ( best['cost'], best['vector'])
    

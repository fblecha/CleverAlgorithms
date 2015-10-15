package main 

import (
	"fmt"
	"math"	
	"math/rand"
)

type Candidate struct {
	cost float64
	vector [][]int
}


func euc_2d(c1 []int, c2 []int) float64 {
	//TODO round
	a := math.Pow(float64(c1[0] - c2[0]), 2)
	b := math.Pow(float64(c1[1] - c2[1]), 2)
	return math.Sqrt(  a + b )
}


func random_permutation(cities [][]int) [][]int {
	perm := make([][]int, len(cities))
	copy(perm, cities)
	for i := range perm {
		r := rand.Intn( len(perm) - i) + i
		perm[r], perm[i] = perm[i], perm[r]
	}
	return perm
}


func main() {
	berlin52 := [][]int {
				{565,575},{25,185},{345,750},{945,685},{845,655},
   {880,660},{25,230},{525,1000},{580,1175},{650,1130},{1605,620},
   {1220,580},{1465,200},{1530,5},{845,680},{725,370},{145,665},
   {415,635},{510,875},{560,365},{300,465},{520,585},{480,415},
   {835,625},{975,580},{1215,245},{1320,315},{1250,400},{660,180},
   {410,250},{420,555},{575,665},{1150,1160},{700,580},{685,595},
   {685,610},{770,610},{795,645},{720,635},{760,650},{475,960},
   {95,260},{875,920},{700,500},{555,815},{830,485},{1170,65},
   {830,610},{605,625},{595,360},{1340,725},{1740,245},}
	
	max_iterations := 100
	max_no_improv := 50
	best := search(berlin52, max_iterations, max_no_improv)
	fmt.Printf("Done. Best Solution: c = %v, v=%v \n", best.cost, best.vector)	
	
}

func cost(permutation [][]int, cities [][]int) float64 {
	distance := 0.0
	for i := range permutation {
		var c2 []int
		if i == (len(permutation) - 1) {
			c2 = permutation[0]
		} else {
			c2 = permutation[i+1]
		}
		distance += euc_2d(cities[i], c2)
	} 
	
	return distance
}


func stochastic_two_opt(permutation [][]int) [][]int {
	perm := make([][]int, len(permutation))
	copy(perm, permutation)
	c1 := rand.Intn(len(perm)) 
	c2 := rand.Intn(len(perm))
	exclude := []int { c1 }
	if c1==0 {
		exclude = append(exclude, len(perm) - 1)
	} else {
		exclude = append(exclude, c1 - 1)
	}
	
	if c1 == (len(perm) - 1) {
		exclude = append(exclude, 0)
	} else {
		exclude = append(exclude, c1 + 1)
	}

	if c2 < c1 {
		c1, c2 = c2, c1
	}
	//perm[c1:c2] = perm[c1:c2] //TODO reverse RHS
	return perm
}


func local_search(best Candidate, cities [][]int, max_no_improv int) Candidate {
	for count := 0; count < max_no_improv; {
		candidate := Candidate{}
		
		candidate.vector = stochastic_two_opt(best.vector)
		
		candidate.cost = cost(candidate.vector, cities)
		if candidate.cost < best.cost {
			count = 0
		} else {
			count++
		}
	}
	return best
}


func double_bridge_move(perm [][]int) [][]int {
	pos1 := 1 + rand.Intn(len(perm) / 4)
	pos2 := pos1 + 1 + rand.Intn(len(perm) / 4)
	pos3 := pos2 + 1 + rand.Intn(len(perm) / 4)
	a := perm[0:pos1]
	b := perm[pos3:len(perm)]
	p1 := append(a, b...)
	c := perm[pos2:pos3]
	d := perm[pos1:pos2]
	p2 := append(c, d...)
	return append(p1,p2...) 	
}


func perturbation(cities [][]int, best Candidate) Candidate {
	candidate := Candidate{}
	candidate.vector = double_bridge_move(best.vector)
	candidate.cost = cost(candidate.vector, cities)
	return candidate
}


func search(cities [][]int, max_iterations int, max_no_improv int) Candidate {
	best := Candidate{}
	best.vector = random_permutation(cities)
	best.cost = cost(best.vector, cities)
	best = local_search(best, cities, max_no_improv)
	for i := 0; i < max_iterations; i++ {
		candidate := perturbation(cities, best)
		candidate = local_search(candidate, cities, max_no_improv)
		if candidate.cost < best.cost {
			best = candidate
		}	 
		fmt.Printf("> iteration %v, best = %v \n", i+1, best.cost)
	}
	return best
}




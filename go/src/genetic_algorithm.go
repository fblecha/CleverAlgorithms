package main

import (
	"fmt"
	"math"
	"math/rand"
	"sort"
)

type Candidate struct {
	fitness int
	bitstring []byte
}

type Population []Candidate

func (s Population) Len() int  { return len(s) }
func (s Population) Swap(i, j int) { s[i], s[j] = s[j], s[i] }
func (s Population) Less(i, j int) bool { return s[i].fitness < s[j].fitness }
func (s Population) Sort() { sort.Sort(s) }


func onemax(bitstring []byte) int {
	sum := 0
	for i := range bitstring {
		if bitstring[i] == 1 {
			sum += 1
		}
	}
	return sum
}

func random_bitstring(num_bits int) []byte {
	bits := make([]byte, num_bits)
	for i:=0; i < len(bits); i++ {
		bits[i] = byte(rand.Intn(1))
	}
	return bits
}

func binary_tournament(pop Population) Candidate {
	i, j := rand.Intn(len(pop)), rand.Intn(len(pop))
	for ; j == i; {
		j = rand.Intn(len(pop))
	}
	if pop[i].fitness > pop[j].fitness {
		return pop[i]
	} else {
		return pop[j]
	}
	
}


func point_mutation(bitstring []byte, p_mutation float64) []byte {
	rate := p_mutation
	child := make([]byte, len(bitstring))
	for i := range bitstring {
		bit := bitstring[i]
		if rand.Float64() < rate {
			if bit == 1 {
				child[i] = 0
			} else {
				child[i] = 1
			}
		} else {
			child[i] = bit
		}
	}
	return child
}

func crossover(parent1 []byte, parent2 []byte, rate float64) []byte {
	if( rand.Float64() >= rate ) {
		return parent1
	}
	point := 1 + rand.Intn(len(parent1) - 2)
	front := parent1[:point]
	back := parent2[point:len(parent1)]
	return append(front, back...)
}


func reproduce(selected Population, pop_size int, p_cross float64, p_mutation float64) Population {
	children := make([]Candidate, 0)
	for i := range selected {
		var p2 Candidate
		if math.Mod(float64(i), 2.0) == 0 {
			p2 = selected[i+1]
		} else {
			p2 = selected[i-1]
		}
		child := Candidate{}
		child.bitstring = crossover(selected[i].bitstring, p2.bitstring, p_cross)
		child.bitstring = point_mutation(child.bitstring, p_mutation)
		children = append(children, child)
		if len(children) >= pop_size {
			break
		}
	}
	return children
}


func search(max_gens int, num_bits int, pop_size int, p_crossover float64, p_mutation float64) Candidate {
	var population Population = make(Population, pop_size)
	for i := range population {
		population[i].bitstring = random_bitstring(num_bits)
		population[i].fitness = onemax(population[i].bitstring)
	}

	population.Sort()
	best := population[0]
	for gen := 0; gen < max_gens; gen++ {
		selected := make(Population, pop_size)
		for i := range selected {
			selected[i] = binary_tournament(population)
		}
		children := reproduce(selected, pop_size, p_crossover, p_mutation)
		for i := range children {
			children[i].fitness = onemax(children[i].bitstring)
		}
		children.Sort() 
		if children[0].fitness >= best.fitness {
			best = children[0]
		}
		population = children
		fmt.Printf("> gen %v, best: %v, %v \n", gen, best.fitness, best.bitstring)
		if best.fitness == num_bits {
			break
		}
	}
	return best
}

func main() {
	num_bits := 64
	max_gens := 100
	pop_size := 100
	p_crossover := 0.98
	p_mutation := 1.0 / float64(num_bits)
	best := search(max_gens, num_bits, pop_size, p_crossover, p_mutation)
	fmt.Printf("done! Solution: f=%v, s=%v \n", best.fitness, best.bitstring)	
}

  
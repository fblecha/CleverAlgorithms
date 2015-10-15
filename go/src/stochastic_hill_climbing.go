package main 

import (
	"fmt"
	"math/rand"
)

type Candidate struct {
	cost int
	vector []byte
}


func onemax(candidate Candidate) int {
	result := 0
	for i := range candidate.vector {
		if candidate.vector[i] == 1 {
			result = result + 1
		} else {
			result = result + 0 //not necessary, but included because it matches the ruby implementation
		}
	}
	return result
}

func random_bitstring(num_bits int) []byte {
	new_vector := make([]byte, num_bits)
	for i := range new_vector {
		if rand.Float64() < 0.5 {
			new_vector[i] = 1
		} else {
			new_vector[i] = 0		
		}
		
	}
	return new_vector
}


func random_neighbor(bitstring []byte) []byte {
	mutant := make([]byte, len(bitstring))
	copy(mutant, bitstring)
	pos := rand.Intn(len(mutant))
	if mutant[pos] == 1 {
		mutant[pos] = 0
	} else {
		mutant[pos] = 1
	}
	return mutant
}

func search(max_iterations int, num_bits int) Candidate {
	candidate := Candidate{}
	candidate.vector = random_bitstring(num_bits)
	candidate.cost = onemax(candidate)
	for i := 0; i < max_iterations; i++ {
		neighbor := Candidate{}
		neighbor.vector = random_neighbor(candidate.vector)
		neighbor.cost = onemax(neighbor)
		if neighbor.cost >= candidate.cost {
			candidate = neighbor
		}
		fmt.Printf("> iteration %v, best=%v \n", i+1, candidate.cost)
		if candidate.cost == num_bits {
			break
		}
	}	
	return candidate
}


func main() {
	num_bits := 64
	max_iterations := 1000
	best := search(max_iterations, num_bits)
	fmt.Printf("Done. Best Solution: c=%v, v=%v \n", best.cost, best.vector)
}




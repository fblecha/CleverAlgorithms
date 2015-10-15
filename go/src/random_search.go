package main

import (
	"fmt"
	"math"
	"math/rand"
)

type Candidate struct {
	cost float64
	vector []float64
}

func objective_function(candidate Candidate) float64 {
  return ( math.Pow(candidate.vector[0],2) + math.Pow(candidate.vector[1],2) )	
}

func random_vector(minmax [][]float64) []float64 {
	new_vector := make([]float64, len(minmax))
	for i := range minmax {
		new_vector[i] = minmax[i][0] + ((minmax[i][1] - minmax[i][0]) *  rand.Float64())
	} 
	return new_vector
}

func search(search_space [][]float64, max_iter int) *Candidate {
	var best *Candidate = nil
	for i := 0; i < max_iter; i++ {
		candidate := Candidate{}
		candidate.vector = random_vector(search_space)
		candidate.cost = objective_function(candidate)
		if best == nil   {
			best = &candidate
		} else if candidate.cost < best.cost {
			best = &candidate
		}
	}
	return best
}


func main() {
  problem_size := 2
  max_iter := 10
  search_space := make([][]float64, problem_size)
  for i := range search_space {
  	search_space[i] = []float64{-5, 5}
  }
  var best *Candidate = search(search_space, max_iter)
  fmt.Printf("Done. Best Solution: c=%v, v=%v", best.cost, best.vector)
}

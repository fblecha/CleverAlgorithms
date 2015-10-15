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
	result := 0.0
	for i := range candidate.vector {
		result = result + math.Pow(candidate.vector[i], 2)
	}
	return result
}

func rand_in_bounds(min float64, max float64) float64 {
	return min + ((max - min) * rand.Float64() )
}


func random_vector(minmax[][]float64) []float64 {
	new_vector := make([]float64, len(minmax))
	for i:= range minmax {
		new_vector[i] = rand_in_bounds(minmax[i][0], minmax[i][1])		
	}
	return new_vector
}

func take_step(minmax [][]float64, current []float64, step_size float64) []float64 {
	position := make([]float64, len(current))
	for i := 0; i < len(position); i++ {
		min := math.Max(minmax[i][0], current[i] - step_size)
		max := math.Min(minmax[i][1], current[i] + step_size)
		position[i] = rand_in_bounds(min, max)
	}
	return position
}


func large_step_size(iter int, step_size float64, s_factor float64, l_factor float64, iter_mult int) float64 {
	if iter > 0 && int(math.Mod(float64(iter), float64(iter_mult))) == 0 {
		return step_size * l_factor
	} else {
		return step_size * s_factor
	}
}


func take_steps(bounds [][]float64, current Candidate, step_size float64, big_stepsize float64) (Candidate, Candidate) {
	step := Candidate{}
	big_step := Candidate{}
	step.vector = take_step(bounds, current.vector, step_size)
	step.cost = objective_function(step)
	big_step.vector = take_step(bounds, current.vector, big_stepsize)
	big_step.cost = objective_function(big_step)
	return step, big_step
}

func search(max_iter int, bounds [][]float64, init_factor float64, 
			s_factor float64, l_factor float64, iter_mult int, max_no_impr int) Candidate {
	
	step_size := (bounds[0][1] - bounds[0][0]) * init_factor
	current := Candidate{}
	count := 0
	current.vector = random_vector(bounds)
	current.cost = objective_function(current)
	for i := 0; i < max_iter; i++ {
		big_stepsize := large_step_size(i, step_size, s_factor, l_factor, iter_mult)
		step, big_step := take_steps(bounds, current, step_size, big_stepsize)
		if step.cost <= current.cost || big_step.cost <= current.cost {
			if big_step.cost <= step.cost {
				step_size, current = big_stepsize, big_step			
			} else {
				current = step
			}
			count = 0
		} else {
			count += 1
			if count >= max_no_impr {
				count = 0
				step_size = (step_size / s_factor)
			}
		}
		fmt.Printf(" > iteration %v, %v\n", i+1, current.cost)
	}
	return current
}

func main() {
	problem_size := 2
	bounds := make([][]float64, problem_size)
	for i := range bounds {
		bounds[i] = []float64{-5, 5}
	}
	max_iter := 1000
	init_factor := 0.05
	s_factor := 1.3
	l_factor := 3.0
	iter_mult := 10
	max_no_impr := 30
	best := search(max_iter, bounds, init_factor, s_factor, l_factor, iter_mult, max_no_impr)
	fmt.Printf("Done. Best solution: c=%v, v=%v", best.cost,best.vector)
}

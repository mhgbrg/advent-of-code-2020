package main

import (
	"strconv"
	"strings"
)

type Cup struct {
	label int
	// next as in "cup that is next in the ring"
	next *Cup
	// prev as in "cup with label-1"
	prev *Cup
}

func main() {
	numCups := 1000000
	iterations := 10000000

	startingState := []int{2, 5, 3, 1, 4, 9, 8, 6, 7}
	ring := constructRing(startingState, numCups)

	for i := 0; i < iterations; i++ {
		ring = move(ring)
		if i % (iterations / 10) == 0 {
			println("iteration", i)
		}
	}

	cup := ring
	for cup.label != 1 {
		cup = cup.next
	}

	println(cup.next.label, "*", cup.next.next.label, "=", cup.next.label * cup.next.next.label)
}

func constructRing(startingState []int, maxLabel int) *Cup {
	n := maxLabel

	// Create the cups, store them in an array ordered in in order of increasing label [1 .. n]
	cups := make([]*Cup, n)
	for i := 0; i < n; i++ {
		cups[i] = &Cup{
			label: i+1,
			next:  nil,
			prev:  nil,
		}
	}

	// Set up prev links, linking each cup to the one with label-1
	for i, cup := range cups {
		j := i-1
		if j == -1 {
			j = n-1
		}
		cup.prev = cups[j]
	}

	// Set up next links, temporary linking each cup to the one with label+1
	for i, cup := range cups {
		j := i+1
		if j == n {
			j = 0
		}
		cup.next = cups[j]
	}

	// Set up correct next links for starting state
	for i, label := range startingState {
		cup := cups[label-1]
		var nextLabel int
		if i == len(startingState)-1 {
			// If last cup in starting state, link to 10
			nextLabel = 10
		} else {
			// Else link to next cup in starting state
			nextLabel = startingState[i+1]
		}
		next := cups[nextLabel-1]
		cup.next = next
	}

	// Link last cup to the cup that is first according to the starting state
	first := cups[startingState[0]-1]
	cups[n-1].next = first

	// Return the cup that is first in the ring
	return first
}

func move(current *Cup) *Cup {
	target := findTarget(current)

	firstToMove := current.next
	lastToMove := current.next.next.next
	firstAfterMoved := lastToMove.next

	lastToMove.next = target.next
	target.next = firstToMove
	current.next = firstAfterMoved

	return current.next
}

func findTarget(current *Cup) *Cup {
	nextThree := map[int]bool{current.next.label: true, current.next.next.label: true, current.next.next.next.label: true}
	target := current.prev
	for nextThree[target.label] {
		target = target.prev
	}
	return target
}

func ringStr(first *Cup) string {
	builder := strings.Builder{}
	builder.WriteString(strconv.Itoa(first.label))
	builder.WriteString(" ")
	for cup := first.next; cup != first; cup = cup.next {
		builder.WriteString(strconv.Itoa(cup.label))
		builder.WriteString(" ")
	}
	return builder.String()
}

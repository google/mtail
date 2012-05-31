package main

type Stack struct {
	top *element
}

type element struct {
	value interface{}
	next  *element
}

func (s *Stack) Push(value interface{}) {
	s.top = &element{value, s.top}
}

func (s *Stack) Pop() (value interface{}) {
	if s.top != nil {
		value, s.top = s.top.value, s.top.next
	}
	return
}

package main

type Stack struct {
	top  *element
	size int
}

type element struct {
	value interface{}
	next  *element
}

func (s *Stack) Size() int {
	return s.size
}

func (s *Stack) Top() (value interface{}) {
	if s.top != nil {
		value = s.top.value
	}
	return
}

func (s *Stack) Push(value interface{}) {
	s.top = &element{value, s.top}
	s.size++
}

func (s *Stack) Pop() (value interface{}) {
	if s.top != nil {
		value, s.top = s.top.value, s.top.next
		s.size--
	}
	return
}

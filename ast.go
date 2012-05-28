// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package main

type visitor interface {
	visitStmtList(stmtlistNode)
	visitExprList(exprlistNode)
	visitCond(condNode)
	visitRegex(regexNode)
	visitId(idNode)
	visitCapref(caprefNode)
	visitString(stringNode)
	visitBuiltin(builtinNode)
	visitAdditiveExpr(additiveExprNode)
	visitAssignExpr(assignExprNode)
	visitIndexedExpr(indexedExprNode)
}

type node interface {
	acceptVisitor(visitor)
}

type stmtlistNode struct {
	s        *scope
	children []node
}

func (s stmtlistNode) acceptVisitor(visit visitor) {
	visit.visitStmtList(s)
}

type exprlistNode struct {
	children []node
}

func (e exprlistNode) acceptVisitor(visit visitor) {
	visit.visitExprList(e)
}

type condNode struct {
	cond     node
	children []node
}

func (c condNode) acceptVisitor(visit visitor) {
	visit.visitCond(c)
}

type regexNode struct {
	pattern string
}

func (r regexNode) acceptVisitor(visit visitor) {
	visit.visitRegex(r)
}

type stringNode struct {
	text string
}

func (s stringNode) acceptVisitor(visit visitor) {
	visit.visitString(s)
}

type idNode struct {
	name string
}

func (i idNode) acceptVisitor(visit visitor) {
	visit.visitId(i)
}

type caprefNode struct {
	name  string
	index int
}

func (c caprefNode) acceptVisitor(visit visitor) {
	visit.visitCapref(c)
}

type builtinNode struct {
	name string
	args exprlistNode
}

func (b builtinNode) acceptVisitor(visit visitor) {
	visit.visitBuiltin(b)
}

type additiveExprNode struct {
	lhs node
	rhs node
	op  int
}

func (a additiveExprNode) acceptVisitor(visit visitor) {
	visit.visitAdditiveExpr(a)
}

type assignExprNode struct {
	lhs node
	rhs node
}

func (a assignExprNode) acceptVisitor(visit visitor) {
	visit.visitAssignExpr(a)
}

type indexedExprNode struct {
	lhs   node
	index node
}

func (i indexedExprNode) acceptVisitor(visit visitor) {
	visit.visitIndexedExpr(i)
}

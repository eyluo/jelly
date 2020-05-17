package ast

import (
	"strconv"

	"github.com/eyluo/jelly/lexer"
	"github.com/eyluo/jelly/symbol"
)

const (
	Var ExpType = iota
	Operator
	IntVal

	Assign StmtType = iota
	Return
)

type ExpType int
type StmtType int

type Exp struct {
	Type ExpType

	Sym symbol.T

	Optr lexer.OpType
	E1   *Exp
	E2   *Exp

	Val int
}

type Stmt struct {
	Type StmtType

	Sym symbol.T
	Exp *Exp
}

type Program []*Stmt

func (e *Exp) ToString() string {
	var result string
	switch e.Type {
	case Var:
		result = e.Sym.ToString()
	case Operator:
		result = "(" + e.E1.ToString() + e.Optr.ToString() + e.E2.ToString() + ")"
	case IntVal:
		result = strconv.Itoa(e.Val)
	}

	return result
}

func (s *Stmt) ToString() string {
	var result string
	switch s.Type {
	case Assign:
		result = s.Sym.ToString() + "=" + s.Exp.ToString()
	case Return:
		result = "return " + s.Exp.ToString()
	}

	return result
}

func (p *Program) ToString() string {
	result := "[ "
	for _, s := range *p {
		result += s.ToString() + "; "
	}
	result += "] "
	return result
}

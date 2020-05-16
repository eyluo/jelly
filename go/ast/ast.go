package ast

import (
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
	Exps []*Exp

	Val int
}

type Stmt struct {
	Type StmtType

	Sym symbol.T
	Exp *Exp
}

type Program []*Stmt

func (e *Exp) ToString() string {
	return "foo"
}

func (s *Stmt) ToString() string {
	return "bar"
}

func (p *Program) ToString() string {
	result := "[ "
	for _, s := range *p {
		result += s.ToString()
	}
	result += "] "
	return result
}

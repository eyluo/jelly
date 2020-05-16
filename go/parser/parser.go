package parser

import (
	"github.com/eyluo/jelly/ast"
	"github.com/eyluo/jelly/lexer"
)

type assoc int

const (
	left assoc = iota
	right
)

func ParseStmt(lxr *lexer.T) *ast.Stmt {
	return nil
}

func ParseProgram(lxr *lexer.T) *ast.Program {
	return nil
}

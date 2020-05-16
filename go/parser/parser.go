package parser

import (
	"errors"

	"github.com/eyluo/jelly/ast"
	"github.com/eyluo/jelly/lexer"
)

type assoc int

const (
	left assoc = iota
	right
)

func ParseStmt(lxr *lexer.T) (*ast.Stmt, error) {
	return nil, errors.New("not yet implemented")
}

func ParseProgram(lxr *lexer.T) (*ast.Program, error) {
	return nil, errors.New("not yet implemented")
}

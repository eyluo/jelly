package parser

import (
	"errors"

	"github.com/eyluo/jelly/ast"
	"github.com/eyluo/jelly/lexer"
	"github.com/eyluo/jelly/symbol"
)

type assoc int

const (
	left assoc = iota
	right
)

func Parse(lxr *lexer.T) (*ast.Exp, error) {
	var parseExpr func(minPrec int) (*ast.Exp, error)
	var parseExprPrec func(minPrec int, lhs *ast.Exp) (*ast.Exp, error)
	var parseAtom func() (*ast.Exp, error)

	opInfo := func(op lexer.OpType) (int, assoc) {
		var prec int
		var assc assoc
		switch op {
		case lexer.Pow:
			prec, assc = 3, right
		case lexer.Plus:
			fallthrough
		case lexer.Minus:
			prec, assc = 1, left
		case lexer.Times:
			fallthrough
		case lexer.Divide:
			prec, assc = 2, left
		}
		return prec, assc
	}

	parseExpr = func(minPrec int) (*ast.Exp, error) {
		lhs, err := parseAtom()
		if err != nil {
			return nil, err
		}

		result, err := parseExprPrec(minPrec, lhs)
		if err != nil {
			return nil, err
		}

		return result, nil
	}

	parseExprPrec = func(minPrec int, lhs *ast.Exp) (*ast.Exp, error) {
		var result *ast.Exp
		tok := lxr.Peek()
		switch tok.Type {
		case lexer.Delim:
			fallthrough
		case lexer.Eof:
			fallthrough
		case lexer.IntVal:
			lxr.Drop()
			result = lhs
		case lexer.Operator:
			prec, assc := opInfo(tok.Optr)
			if prec < minPrec {
				return lhs, nil
			}

			lxr.Drop()

			var nextMinPrec int
			if assc == left {
				nextMinPrec = prec + 1
			} else {
				nextMinPrec = prec
			}

			rhs, err := parseExpr(nextMinPrec)
			if err != nil {
				return nil, err
			}

			temp := &ast.Exp{
				Type: ast.Operator,
				Optr: tok.Optr,
				E1:   lhs,
				E2:   rhs,
			}

			return parseExprPrec(minPrec, temp)
		default:
			result = lhs
		}

		return result, nil
	}

	parseAtom = func() (*ast.Exp, error) {
		tok := lxr.Pop()
		switch tok.Type {
		case lexer.IntVal:
			return &ast.Exp{
				Type: ast.IntVal,
				Val:  tok.Val,
			}, nil
		case lexer.Symbol:
			return &ast.Exp{
				Type: ast.Var,
				Sym:  symbol.New(tok.Sym),
			}, nil
		case lexer.LParen:
			exp, err := parseExpr(0)
			if err != nil {
				return nil, err
			}

			tok2 := lxr.Pop()
			if tok2.Type != lexer.RParen {
				return nil, errors.New("unbalanced parentheses")
			}

			return exp, nil
		default:
			return nil, errors.New("illegal atom")
		}
	}

	return parseExpr(0)
}

func ParseStmt(lxr *lexer.T) (*ast.Stmt, error) {
	tok := lxr.Pop()
	switch tok.Type {
	case lexer.Symbol:
		eq := lxr.Pop()
		switch eq.Type {
		case lexer.Eq:
			exp, err := Parse(lxr)
			if err != nil {
				return nil, err
			}

			return &ast.Stmt{
				Type: ast.Assign,
				Sym:  symbol.New(tok.Sym),
				Exp:  exp,
			}, nil
		default:
			return nil, errors.New("assignment should be followed by =")
		}
	case lexer.Return:
		exp, err := Parse(lxr)
		if err != nil {
			return nil, err
		}

		return &ast.Stmt{
			Type: ast.Return,
			Exp:  exp,
		}, nil
	case lexer.Delim:
		return ParseStmt(lxr)
	default:
		return nil, errors.New("statement does not begin with assignment or return")
	}
}

func ParseProgram(lxr *lexer.T) (ast.Program, error) {
	tok := lxr.Peek()
	switch tok.Type {
	case lexer.Eof:
		return make([]*ast.Stmt, 0), nil
	default:
		stmt, err := ParseStmt(lxr)
		if err != nil {
			return nil, err
		}

		result, err := ParseProgram(lxr)
		if err != nil {
			return nil, err
		}

		return append([]*ast.Stmt{stmt}, result...), nil
	}
}

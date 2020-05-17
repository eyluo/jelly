package lexer

import (
	"errors"
	"io/ioutil"
	"strconv"
	"unicode"
)

type OpType int
type TokenType int

const (
	// Op (operators)
	Pow OpType = iota
	Plus
	Minus
	Times
	Divide

	// Tok (tokens)
	Symbol TokenType = iota
	IntVal
	Operator
	Eq
	LParen
	RParen
	Delim
	Return
	Eof
)

type Token struct {
	Type TokenType
	Optr OpType
	Sym  string
	Val  int
}

type T struct {
	idx    int
	tokens []*Token
}

func (op OpType) ToString() string {
	switch op {
	case Pow:
		return "^"
	case Plus:
		return "+"
	case Minus:
		return "-"
	case Times:
		return "*"
	default:
		return "/"
	}
}

func (tok *Token) ToString() string {
	switch tok.Type {
	case Symbol:
		return tok.Sym
	case IntVal:
		return strconv.Itoa(tok.Val)
	case Operator:
		return tok.Optr.ToString()
	case Eq:
		return "="
	case LParen:
		return "("
	case RParen:
		return ")"
	case Delim:
		return ";"
	case Return:
		return "return"
	default:
		return "EOF"
	}
}

func newLexer(file string) (*T, error) {
	result := &T{}
	flength := len(file)

	var lexToTokens func(int) ([]*Token, error)
	lexToTokens = func(i int) ([]*Token, error) {
		if i == flength {
			return []*Token{&Token{Type: Eof}}, nil
		}

		ch := rune(file[i])

		var token *Token
		if unicode.IsSpace(ch) {
			return lexToTokens(i + 1)
		} else if unicode.IsDigit(ch) {
			temp := ""
			for i < flength {
				temp_ch := rune(file[i])
				if unicode.IsDigit(temp_ch) {
					temp += string(temp_ch)
				} else if unicode.IsLetter(temp_ch) {
					return nil, errors.New("invalid character encountered while lexing int")
				} else {
					break
				}
				i++
			}

			// HERE: i'm pretty sure this needs to be here so we lex the next character.
			i--

			tempInt, err := strconv.Atoi(temp)
			if err != nil {
				return nil, errors.New("failed to lex int")
			}
			token = &Token{Type: IntVal, Val: tempInt}
		} else if unicode.IsLetter(ch) {
			temp := ""
			for i < flength {
				temp_ch := rune(file[i])
				if unicode.IsDigit(temp_ch) || unicode.IsLetter(temp_ch) {
					temp += string(temp_ch)
				} else {
					break
				}
				i++
			}

			// HERE: i'm pretty sure this needs to be here so we lex the next character.
			i--

			// TODO: add more keyword checks here.
			switch temp {
			case "return":
				token = &Token{Type: Return}
			default:
				token = &Token{Type: Symbol, Sym: temp}
			}
		} else {
			switch ch {
			case '=':
				token = &Token{Type: Eq}
			case ';':
				token = &Token{Type: Delim}
			case '^':
				token = &Token{Type: Operator, Optr: Pow}
			case '+':
				token = &Token{Type: Operator, Optr: Plus}
			case '-':
				token = &Token{Type: Operator, Optr: Minus}
			case '*':
				token = &Token{Type: Operator, Optr: Times}
			case '/':
				token = &Token{Type: Operator, Optr: Divide}
			case '(':
				token = &Token{Type: LParen}
			case ')':
				token = &Token{Type: RParen}
			default:
				return nil, errors.New("unrecognized character in lexing")
			}
		}

		remainder, err := lexToTokens(i + 1)
		if err != nil {
			return nil, err
		}

		return append([]*Token{token}, remainder...), nil
	}

	result.idx = 0
	tokens, err := lexToTokens(0)
	if err != nil {
		return nil, err
	}

	result.tokens = tokens

	return result, nil
}

func NewLexer(filename string) (*T, error) {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}

	return newLexer(string(content))
}

func (lexer *T) Pop() *Token {
	if lexer.idx == len(lexer.tokens)-1 {
		return lexer.Peek()
	}
	result := lexer.Peek()
	lexer.idx++
	return result
}

func (lexer *T) Peek() *Token {
	return lexer.tokens[lexer.idx]
}

func (lexer *T) Drop() {
	lexer.Pop()
}

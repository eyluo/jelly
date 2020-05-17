package parser

import (
	"testing"

	"github.com/eyluo/jelly/lexer"
)

func TestParser(t *testing.T) {
	tests := []string{
		"../../tests/statements.test",
		"../../tests/pemdas3.test",
		"../../tests/abc.test",
	}

	for _, test := range tests {
		lxr, err := lexer.NewLexer(test)
		if err != nil {
			t.Fatal(err)
		}

		prog, err := ParseProgram(lxr)
		if err != nil {
			t.Fatal(err)
		}

		t.Log(prog.ToString())
	}
}

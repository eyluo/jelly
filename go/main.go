package main

import (
	"fmt"
	"os"

	"github.com/eyluo/jelly/typecheck"

	"github.com/eyluo/jelly/lexer"
	"github.com/eyluo/jelly/parser"
)

func main() {
	args := os.Args
	if len(args) < 3 {
		fmt.Printf("usage: jelly <file_name> <prog_name>")
		return
	}
	in, _ := os.Args[1], os.Args[2]
	lxr, err := lexer.NewLexer(in)
	if err != nil {
		fmt.Printf(err.Error())
		return
	}

	prog, err := parser.ParseProgram(lxr)
	if err != nil {
		fmt.Printf(err.Error())
		return
	}

	err = typecheck.Scan(prog)
	if err != nil {
		fmt.Printf(err.Error())
		return
	}

	// TODO: add the remainder of the compilation pipeline.
}

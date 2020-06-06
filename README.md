# Jelly

### Learning about compilers by implementing one.

This is __Jelly__, a compiler project I'm working on to learn about compiler theory, design, and implementation.

Jelly is actively maintained in Ocaml, and I'm dabbling in a Go implementation on the side.

## Pipeline

Jelly reads in programs (denoted as `.test` files), and is currently capable of outputting a very limited subset of x86-64 assembly, specifically for arithmetic involving parentheses, multiplication, addition, and subtraction (PMAS).

That being said, the pipeline is composed of a handful of components described below:

1. The __lexer__ reads in a program file as a character stream and creates a token stream.
1. The __parser__ reads in a token stream and constructs an _abstract syntax tree_ (__AST__) to represent the program.
1. The __typechecker__ verifies specific rules for the language (typing and variable declaration, for two).
1. The __IR3__ module lowers the _AST_ into _triple intermediate representation_ (__triple IR__), in which every instruction is of the form `op arg1 arg2`.
1. The __IR2__ module further lowers _triple IR_ into _double intermediate representation_, in which every instruction is of the form `arg1 <- op arg2`.
1. The __liveness__ module performs a livescan of the _IR2_ program, identifying periods at which temporary variables exist. This module creates what can be thought of as an _interference graph_, in which nodes represent _IR2_ temporaries, and edges between nodes denote two temporaries existing simultaneously.
1. The __coloring__ module takes an interference graph and finds an optimal graph coloring by node that corresponds to specific registers in the final assembly program. Coloring works by first determining an optimal ordering of nodes (temporaries) for a _chordal graph_ before coloring using this ordering.
1. I guess we'll find out!

### Fun ASCII Diagram
```
                                 [Typechecker]
                                       |
FILE -[Lexer]-> Tokens -[Parser]-> AST -[Triple Lowering]-> IR3
                                                             |
                                                      [Double Lowering]
                                                             |
                                                             v
   [Graph Coloring]- Interference Graph <-[Liveness Check]- IR2
          |                                                  |
          v                                                  |
    Colored Graph ---------[Mysterious step(s)!]--------------
                                     |
                                     v
                               PROGRAM BINARY
                                                             
```
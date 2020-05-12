# Parsing

- __Goal:__ turn a stream of characters into an abstract syntax tree.

```
int main() {
    if (x >= 3) {
        ...
    }
}

Function(return_type = int, name = symbol "main", body = Block(If(GreaterThan(symbol "x", IntLit 3), Block(...)), ...))
```

- PEMDAS is hard?
- Parsing breaks into a __lexer__ and __parser__
    - `Lexer : char stream -> token stream, Parser : token stream -> AST`
- Input: stream of characters... __how to handle?__
    - Character by character? Inefficient because you'd have to learn and identify what you're looking at as you see each new character. 
    - This turns out to be extremely hard. Too much state to keep track of!

### Lexer

- __Solution:__ add a lexer!
    - Linearly scans input and "tokenizes" into chunks that are easier to parse with.
    - Also handles whitespace and comments.

```
int main() {
    if (x >= 3) {
        ...
    }
}

Symbol("int") Symbol("main") LParen RParen LBracket If LParen Symbol("x") ... EOF
```

- The lexer notably got rid of whitespace, grouped symbols into strings, and tokenized operators + keywords.
    - When a lexer sees a letter, it should lex as a __symbol__ until the symbol is complete, at which point you check if it's a keyword.

### Parser

- Takes in the lexed stream and returns an __AST__: abstract syntax tree.
- Recursive descent parsing?

#### Interface between lexer and parser

```
type lexer
val pop : lexer -> token
val peek : lexer -> token
val drop : lexer -> unit
```

- Parser "lookahead": how many tokens do I have to look ahead at in order to know what's going on?
    - If your grammar is ambiguous, then this question doesn't have an answer because there are multiple "correct" answers.
    - Undesirable: infinite lookahead.
    - `Foo < Bar, Baz > qux;` two comparisons or one template instantiation?
    - `LL(1)` is optimal: lookahead of 1.
    - Minimal lookahead is a property of the grammar of the language.
    - This lexer-parser interface is clean because you can only have `LL(1)`.
    - Don't have big lookahead with designing languages!

- Parsing typically involves your good friend: __mutual recursion!__
- Operator-precedence parsing
    - __Key idea:__ 

```ocaml
type Operator = Plus | Minus | Div | Times

type token =
    | Operator of op
    | IntLit of int
    | LParen | RParen

(* AST *)
type exp = 
    | IntVal of int
    | Binop of op * exp * op

(* grammar *)
atom : 
    | IntLit
    | LParen, exp, RParen

exp :
    | atom
    | lhs = atom, op = Operator, rhs = exp

parse_expr : lexer -> exp

let parse_expr lexer = 
```
```
def parse_atom:
    tok = pop lexer
    switch tok:
        case IntLit
            return IntVal
        case LParen
            e = parse_expr
            tok2 = pop lexer
            assert(tok2 = RParen) // for mismatched paren
            return e

def parse_expr:
    lhs = parse_atom
    tok = pop lexer
    switch tok:
        case Eof:
            return lhs
        case IntLit:
            return error
        case Operator:
            rhs = parse_expr
            return Operator(op, lhs, rhs)

```
- This parser is right-recursive! (try tracing)
    - `1 + 2 * 3` okay, but `1 * 2 + 3` bad!
    - Key insight: let `parse_expr` take min prec!
```
def parse_atom:
    tok = pop lexer
    switch tok:
        case IntLit
            return IntVal
        case LParen
            e = parse_expr(1) # minimum prec, reset for parentheses
            tok2 = pop lexer
            assert(tok2 = RParen) // for mismatched paren
            return e

def parse_expr:
    while there's more stuff:
        lhs = parse_atom
        tok = pop lexer
        switch tok:
            case Eof:
                return lhs
            case IntLit:
                return error
            case Operator:
                rhs = parse_expr (precedence(op))
                return Operator(op, lhs, rhs)
```

# Register allocation
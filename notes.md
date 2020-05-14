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

# Type-checking

For now, focused on two things:

- Ensure that variables are not used before they are defined
- Ensure the last statement is a return statement and that no other statement is a return statement

```ocaml
val typecheck : Ast.program -> unit
```

- Later on, type-checking will include __sizing__ (structs, user-defined classes, etc.) and __type inference__.

```ocaml
type 'a decl = 
    | FunDecl of type * symbol * (symbol * ty) list
    | StructDecl of 'a * size * (symbol * ty) list

(* common trick for not duplciating an entire datatype when you want to add more
 * information as passes continue. *)
val typecheck : unit Ast.program -> Size.t Ast.program
```

# IR

```ocaml
type exp = 
    | IntVal of int
    | Var of symbol
    | BinOp of op * exp * exp

type stmt = 
    | Assign of symbol * exp
    | Return of exp
```

We kind of have this so far. How far are we from actual machine code?

One statement (code granularity) vs. one assembly instruction (machine granularity)

- `Return of exp` --> how to return?
- Where things are stored. (see __register allocation__)
- How many things can be done at a time?
    - `Assign(x, BinOp(Mul, BinOp(Add, y, z, ), w))`

### IR - intermediate represenation
- Compiler IR corresponds to any intermediate language that code is converted to during the compilation pipeline.
- "Triple IR" differs from regular assembly:
    - Differs in assuming infinite number of registers (temporaries)
    - Similar in doing only one operation at a time

- Triple IR has two operations (`mov` and `binop`) and non-recursive operands:
```
Operations
dst <- src
dst <- op1 # op2 (this is the namesake for triple IR)

Operands
Immediates: ints, etc.
Temporaries: temporary register value (later, this will be either a hardware register or memory location)
```
Example
```
a = 5;
b = 2 * 3 + 1;
c = b * a * a;
b = a * 2;
return b + a + c;
```
in IR form...
```
%t1 <- 5            # a
%t2 <- 2 * 3
%t3 <- %t2 + 1      # b
%t4 <- %t1 * %t1
%t5 <- %t3 * %t4    # c
%t6 <- %t1 * 2      # b
%t7 <- %t6 + %t1
return %t7 + %t5    # return
```

__Static single assignment (SSA)__ form: never assign a variable more than twice.
- Good for straight-line optimization!
- Harder to dictate when control flow is involved.
- Wouldn't loops contradic SSA?
    - Use phi-function or phi-node.
    - Basic blocks: straight-line code with a single (un)conditional jump at the end.
    - A phi-node is a function that "selects" what `%t` something should be based on __the basic block we came from__.

```
x, y, = 1, 2;
while (true) { x = x + y; }

bb1:
    %t1 = 1
    %t2 = 2
    jmp bb2
bb2:
    %t3 = %t1 + %t2
    jmp bb2
```

```ocaml
(* Returns a list of inst that compute the expression, storing the result in dst. *)
let lower_exp (dst : Temp.t) (exp : Ast.exp) : Ir.stmt list = ...

lower_exp (dst = %t1) (exp = IntVal 5) --> [ %t1 <- 5 ]
lower_exp (dst = %t2) (exp = BinOp(Plus, IntVal 5, IntVal 6)) -> 
    [%t3 <- 5 ; %t4 <- 6 ; %t2 <- %t3 + %t4]
```

To try to actually turn IR into usable assembly:
``` ocaml
type operand = Temp of Temp.t | Immediate of int
```

```
def lower_operand
    temps -> -(8 * temp #)%rbp
    immediates -> $immediate

assign(lhs, rhs) -> mov (lower_operand rhs) %rax; mov %rax (lower_operand lhs)
binop() -> ??? # left as an exercise for the reader
return() -> ??? # left as an exercise for the reader
```

# Register allocation

### Using hashtables
- Key: `Symbol.t`
- Typechecking for defined variables
- mapping variables to temporaries
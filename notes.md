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

# Error messages

```ocaml
module Mark = 
    type 'a t

    (* object -> startPosition -> endPosition -> mark *)
    val create : 'a -> int * int -> int * int -> 'a t
    val obj : 'a t -> 'a
    val map : 'a t -> f:('a -> 'b) -> 'b t
    val start
    val stop
    (* steal metadata in 'b t *)
    val with_mark : 'a -> 'b t -> 'a t 
    (* steal start of 'a, stop of 'a, and assign to 'c *)
    val create_from_range : 'a t -> 'b t -> 'c -> 'c t 
    (* Mark is our functor because Mark takes a type t and transforms (lifts) it
     * to be a marked 'a. *)

     (* lifting operation lifts the transformation f from the realm of objects
      * to the realm of "marked" objects. *)
     val f : 'a -> 'b
     val f' : 'a t -> 'b t


     type exp =
        | BinOp of Lexer.op * mexp * mexp
        ...
     and mexp = exp Mark.t

```

### Addendum on polymorphism

- __Polymorphism__: working with different types
- Subtype polymorphism (doing a thing that works with certain subtypes of a certain type.)
```
class Animal:
    def speak() : String

class Dog : Animal
class Cat : Animal

List animals = List<Animal>()
animals.append(Dog())
animals.append(Cat())
for a in animals: a.speak()
```
- Ad-hoc polymorphism (like so-so kind of version of polymorphism... generally considered a bad idea by language designers because interacts extremely poorly with implicit casting rules.)
```
int foo(String x)
int foo(int x)

foo(3)
```
- Parametric polymorphism (\forall of polymorphism)
```
forall t1, forall t2, map : (t1 -> t2) -> t1 list -> t2 list
```
- Yaron Minsky (of Jane Street fame (so says Henry Nelson)): __FP is good because in writing large programs, you pretty much always want parametric polymorphism as your main way of separating concerns.__

# Register allocation

```
t1 = 3
t2 = 5
t3 = t1 + t2
t4 = t3 + t1
t5 = t3 + t4
return t5
```

- Which temps can be assigned to the same register?
    - t1 and t5 are never used at the same time.

A temporary is __live__ when its value will be used in the future.
A __program point__ is between two instructions.
Two temps __interfere__ when both are live at a given program point.
t1 and t5 do not interfere because they are never alive at the same program point.

Graph coloring:
__P1:__ Given interference data, can the temporaries be reg-allocated with k-registers?
__P2:__ Given a graph, can the graph be colored with k colors? (Color each vertex such that no two vertices with an edge between them are the same color.)
Reduce problem 1 to problem 2.
Pre-2005 paper-worthy challenge: show that the graph generated from the reduction is "chordal." (Chordal graphs can be colored in polynomial time.)

__Solution:__
Reduce problem 1 to problem 2 by creating `n` nodes corresponding to the `n` temporaries. Connect nodes if their temporaries interfere (this is known as an __interference graph__). Perform a graph coloring using `k` colors for `k` registers.

Interference edge (solid line -----------)
Move edge (dashed line - - - - -)
in interference graphs. Treat the same when coloring graph. Try to merge vertices along move edges.

Later: linear allocation and hardware limitations.

So far, we've lowered `ast -> triple IR -> two address IR (x86) -> machine code`.
Where should we do register allocation? Feasibly can be done between `IR`'s or before machine code.
- Register allocation before two-address IR
    - `-` You're doing liveness on a three-address format, but you want to know the interference edges for the eventual two-address format. This isn't impossible, but there are edge cases.
    - `-` Lowering after reg-alloc has more edge cases
        - If we register alloc between `IR`'s, you might accidentally stomp on the destination. Implement a check to make sure that this doesn't happen.
- Register allocation after two-address IR
    - `+` You're doing liveness analysis on the code right before it emitted, so you are more confident that your register alocation will actually be correct.
    - `-` Lowering to two-address form breaks SSA. Lowering from triple to two-address IR goes from side-effect-free to mutating state, and SSA has really nice properties.
    - `-` Lose ability to leverage commutativity to save copies.
        - LLVM solves this by doing "local thinking" to determine which operation to do in 3-to-2 lowering, and pick the one with fewer copies.

### Liveness

```
mov $3, %t1     
(%t1)   
mov $4, %t2     
(%t1, %t2)
mov %t1, %t3    
(%t2, %t3)
add %t2, %t3    
(%t3)
mov %t3, %rax   
(%rax)
```

- Liveness annotation can be implemented in O(n) (single pass).
- Live-range of a temporary begins the first time it's defined
- Live-range of a temporary ends when the temp is no longer being used.
- By iterating backwards, we can annotate every program point!

```python
G = graph()
currently_live = set()
for instr in reversed(program):
    uses = uses(instr)
    defs = defs(instr)
    currently_live.remove_all(defs)
    currently_live.add_all(uses)
    add_interference_edges(G, for all pairs in currently_live)
G.color() # -> Hashtbl[vertex, color]
```

__Observation:__ for all of our programs, `%rax` should be __live-out__ of the basic block.
For a temporary to be __live-in__ of the basic block, it must be __live-out__ of all incoming basic blocks.

### Handling physical register constraints (pre-coloring)

- What if a temp _has_ to be in `%rcx`? How do we show this?
- If you have 16 physical registers, then in addition to all of your nodes for your temporaries and all the edges betweenn them, add a 16-clique of nodes for your physical registers. Now you can add interference nodes between temporaries and physical registers.
- Two parts to register constraints:
    - Temporaries can interfere with physical registers. Adding the 16-clique allows us to directly map from physical registers to colors because every color will only have a unique physical register.
    - Temporaries can interfere with temporaries.

# Control Flow
- Earlier, we discussed basic blocks.
- Envision control flow as a directed graph in which edges are potential jumps.
- Strings of nodes with one in-edge and one-edge can all be collapsed into a basic block, or a "mega-instruction" as Henry calls it.
- Only need to add one type of instruction (`jmp`, `condjmp`), which outputs which basic block to jump to.

### Writing a control flow compiler:
1. Write a one basic block compiler.
2. Run it n times for n basic blocks.
3. ??? (AKA __dataflow analysis__)
4. Profit.


### Using hashtables
- Key: `Symbol.t`
- Typechecking for defined variables
- mapping variables to temporaries

# Booleans

Support `>, >=, <, <=, &&, ||, ==, !=`

(no short circuiting for now)

`>, >=, <, <= : int * int -> bool`
`==, != : 'a * 'a -> bool`
`&&, || : bool * bool -> bool`

`&&, || -> andq, orq`
`==, !=, ... -> cmpq; mov $0, %rax; setcc %ax`

`setcc` must count as a use and define for liveness analysis otherwise the register will never be used. Another solution is to let the IR count the `mov;setcc` as one instruction and emit two assembly instructions when we need to.

How to represent a bool in memory?
Easy way: in registers, bools still take up 64 bits of a register
In memory, bools on the stack should probably only take up 1 byte. Keep in mind, though, that stacks are generally 16-byte aligned (x86-64, 8-byte might be okay).
Lazy method: make bools 8 bytes like ints lmao.

```ocaml
type stmt = 
  | Declare of Symbol.t * ty
  | Assign of Symbol.t * mexp
  | Return of mexp
type mstmt = stmt Mark.t
```

## Type erasure

Front-end concerned with typechecking and representation, and then can be lowered to a more weakly typed version.
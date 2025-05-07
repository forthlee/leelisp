# llisp - A Bytecode Compiler and Interpreter for Lisp in Python

`llisp` is a minimalist Lisp interpreter and bytecode compiler implemented in Python, inspired by the principles outlined in [Bytecode Compilers and Interpreters](https://bernsteinbear.com/blog/bytecode-interpreters/). It compiles Lisp code into an efficient bytecode representation and executes it using a virtual machine (VM). Designed for educational purposes, `llisp` offers a clear and accessible way to explore Lisp and bytecode compilation.

## Features
- **Bytecode Compilation**: Compiles Lisp expressions into bytecode for efficient execution.
- **Python Implementation**: Written in Python for clarity, maintainability, and cross-platform compatibility.
- **Interactive REPL**: Provides a read-eval-print loop for interactive experimentation.
- **Core Lisp Support**: Includes `define`, `lambda`, `macro`, `let*`, `dolist`, and arithmetic operations.
- **Debugging Tools**: Features a `dis` function to display bytecode and supports test suites for validation.
- **Extensible**: Easy to modify and extend due to Python's flexible syntax.

## Installation

### Prerequisites
- Python 3.6 or higher
- POSIX-compatible system (Linux, macOS, or Windows with WSL for full functionality)

### Setup
1. Clone the repository:
   ```bash
   git clone https://github.com/forthlee/llisp.git
   cd llisp
   ```
2. No compilation is required; the project runs directly with Python.

## Usage
Run the interpreter in interactive mode:
```bash
python llisp.py
```
Execute a Lisp file:
```bash
python llisp.py example.lisp
```
Run tests to verify functionality:
```bash
python test.py
```

### Example
Create a file `example.lisp`:
```lisp
(define square (lambda (x) (* x x)))
(print "Square of 5 =" (square 5))
```
Run:
```bash
python llisp.py example.lisp
```
Output:
```
Square of 5 = 25
```

### Interactive REPL Examples
```bash
$ python test.py
********************************************* : 0 out of 79 tests fail.
********************************************* : 0 out of 64 tests fail.

$ python llisp.py
llisp> (dis fib)
fib ('x',)
(GET('x'), CONST(3), CALL('<',2), JMP1(10),
 GET('x'), CONST(1), CALL('-',2), CALL('fib',1),
 GET('x'), CONST(2), CALL('-',2), CALL('fib',1),
 CALL('+',2), JUMP(1), CONST(1))

llisp> (print "fib(10)=" (fib 10))
fib(10)= 55

llisp> (define stack nil)
llisp> (push 1 stack)
llisp> (push 2 stack)
llisp> (push 3 stack)
llisp> stack
[3, 2, 1]
llisp> (pop stack)
3

llisp> (let* ((stack nil))
      (dolist (x '(1 2 3))
            (dolist (y '(a b c))
                (push (list x y) stack)
            )
       )
       stack
      )
[[3, 'c'], [3, 'b'], [3, 'a'], [2, 'c'], [2, 'b'], [2, 'a'], [1, 'c'], [1, 'b'], [1, 'a']]

llisp> (map (lambda (m) (map (lambda (n) (list m n)) (seq 1 4))) (seq 1 4))
[[[1, 1], [1, 2], [1, 3]], [[2, 1], [2, 2], [2, 3]], [[3, 1], [3, 2], [3, 3]]]

llisp> (for ((i '(1 2)) (j '(3 4))) (eq? 5 (+ i j)) (list i j))
[[2, 3], [1, 4]]
llisp> (exit)
```

## How It Works
1. **Parser**: Reads Lisp code and converts it into an abstract syntax tree (AST).
2. **Bytecode Compiler**: Translates the AST into a sequence of bytecode instructions (e.g., `GET`, `CONST`, `CALL`, `JMP`), following techniques described in [Bytecode Compilers and Interpreters](https://bernsteinbear.com/blog/bytecode-interpreters/).
3. **Virtual Machine**: Executes the bytecode, managing a stack and environment for variables and functions.
4. **REPL**: Facilitates interactive execution, with debugging support via the `dis` function to inspect bytecode.

## Supported Features
- **Special Forms**: `define`, `lambda`, `if`, `quote`, `let*`, `dolist`, `for`
- **Built-in Functions**: `+`, `-`, `*`, `/`, `car`, `cdr`, `cons`, `eq?`, `list`, `push`, `pop`, `map`, `seq`, `print`
- **Data Types**: Numbers, symbols, lists, booleans (`true`, `false`), `nil`
- **Debugging**: `dis` for bytecode disassembly
- **Environment**: Supports global and local variable bindings

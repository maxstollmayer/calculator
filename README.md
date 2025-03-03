# Simple CLI Calculator

Evaluate basic arithmetic expressions in the terminal.

Written as an exercise to implement a recursive descent parser combinator in Haskell.

## Usage

Clone the repository using and change into the newly created directory. Using GHC 9.4.8 or later compile with `ghc -isrc --make src/Main.hs -o calculate`. Now run `./calculate [OPTION] [EXPRESSION...]`.

### Options

-h, --help    Shows help message and exits

### Expression

Expressions consist of the following building blocks.

- Integers and floats: 7, 3.14, ...
- Parentheses: (x)
- Addition: x + y
- Subtraction: x - y
- Multiplication: x * y
- Division: x / y
- Exponentiation: x ** y
- Functions: sqrt, log, sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh

### REPL Mode

If no argument is provided, it will enter REPL mode and evaluate expressions interactively.

## References

- https://book.realworldhaskell.org/read/using-parsec.html
- https://jakewheat.github.io/intro_to_parsing/
- https://www.youtube.com/watch?v=N9RUqGYuGfw


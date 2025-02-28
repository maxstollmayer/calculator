# Simple CLI Calculator

Evaluate basic arithmetic expressions in the terminal.

Written as an exercise to implement a recursive descent parser combinator in Haskell.

## Usage

Clone the repository using and change into the newly created directory. Using GHC 9.4.8 or later compile with `ghc -isrc --make src/Main.hs -o calculate`. Now run `./calculate [OPTION] [EXPRESSION...]` with.

### Options

-h, --help    Shows help message and exits

### Expression

Expressions consist of the following building blocks.

- Number  .......... integers and floats
- Parentheses ...... (x)
- Addition ......... x + y
- Subtraction ...... x - y
- Multiplication ... x * y
- Division ......... x / y

### REPL Mode

If no argument is provided, it will enter REPL mode and evaluate expressions interactively.


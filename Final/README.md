# CS 252 Final Project: Prism Language

## Overview
Prism is a custom programming language implemented in Haskell. It combines a small imperative core with powerful functional features, specifically designed for **"Declarative Deconstruction"**—the elegant breakdown of complex data structures. It is optimized for rule engines, symbolic manipulation, data transformation, and dynamic configuration logic.

## Project Structure
- **AST.hs**: Abstract Syntax Tree and Value definitions.
- **Parser.hs**: Parsec-based grammar implementation.
- **Interpreter.hs**: State-based evaluator with monadic error handling.
- **Main.hs**: Command-line interface.
- **DesignDocumentation.md**: Detailed rationale, Clean Code analysis, and formal references.
- **demo.lp**: Main demonstration script.
- **tests/**: A collection of specialized test programs (`recursion_test.lp`, `list_ops.lp`, `error_cases.lp`).

## How to Run
Ensure you have GHC (Glasgow Haskell Compiler) installed.

### Run the Demo
```bash
runghc -iFinal Final/Main.hs Final/demo.lp
```

### Run Specialized Tests
```bash
# Test Recursion (Factorial & Fibonacci)
runghc -iFinal Final/Main.hs Final/tests/recursion_test.lp

# Test List Operations (Reverse & Map)
runghc -iFinal Final/Main.hs Final/tests/list_ops.lp

# Test Error Handling
runghc -iFinal Final/Main.hs Final/tests/error_cases.lp
```

## Features
1. **User-defined Functions**: Full support for recursive calls and local scoping.
2. **Recursive Lists**: Native list literals, `cons` operator, and empty list support.
3. **Pattern Matching**: Robust `match` expressions with variable binding and list deconstruction.
4. **Error Handling**: Graceful reporting of runtime errors (division by zero, unbound variables, type mismatches).

## Authors
- Kushagra Bainsla
- Krishna Mula
- Tejas Chakkarwar

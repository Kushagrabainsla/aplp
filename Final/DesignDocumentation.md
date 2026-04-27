# Prism Language - Design & Implementation Documentation

## 1. Introduction
Prism is a functional-imperative language designed for the CS252 final project. It implements three major features beyond a basic WHILE-language: user-defined functions, recursive list structures, and robust pattern matching.

The name **Prism** reflects the language's core philosophy: just as a prism deconstructs white light into its component colors, the Prism language deconstructs complex data structures into their fundamental parts through elegant pattern matching.

## 2. Motivation
The primary motivation behind Prism is to provide a tool for **"Declarative Deconstruction."** In traditional imperative languages, processing recursive data like lists requires complex loops, index management, and nested conditionals. Prism shifts this burden from the programmer to the language.

### Key Use Cases:
1.  **Rule Engines**: Evaluating complex business or game logic by "matching" shapes of data.
2.  **Symbolic Manipulation**: Ideal for building compilers or mathematical simplifiers where data is often tree-like or recursive.
3.  **Data Transformation**: Cleaning and reshaping complex JSON-like list structures with minimal code.
4.  **Configuration with Logic**: Modern infrastructure tools require "smart" configurations that are more than static text; Prism allows for dynamic setting generation through its functional core.

### 2.1 Lexical Scoping
**Decision:** Function calls use a dedicated local environment.
**Reasoning:** To ensure sound modularity, functions should not have side effects on the global variable space unless explicitly intended. By isolating the `variables` map during function evaluation, we prevent variable name collisions and implement proper scoping rules found in modern languages.

### 2.2 First-Class Pattern Matching
**Decision:** `match` is an expression, not just a way to define functions.
**Reasoning:** While many languages (like Haskell) use pattern matching in function definitions, making `match` an expression allows for more flexible code. A developer can perform case analysis in the middle of a complex calculation without needing to break it out into a separate function.

### 2.3 Recursive List Structure
**Decision:** Lists are implemented as a native `ListVal` in the interpreter.
**Reasoning:** This allows for $O(1)$ `cons` operations and easy recursive deconstruction. Using a native Haskell list as the backing store in the `Value` type ensures that the interpreter remains performant and easy to reason about.

### 2.4 Error Handling with ExceptT
**Decision:** Using `ExceptT String (State InterpreterState)` for evaluation.
**Reasoning:** Standard interpreters often crash on runtime errors (like division by zero or unbound variables). By using a Monad Transformer stack, Prism provides descriptive error messages without crashing the host Haskell environment, which is a key requirement for a "production-ready" academic project.

## 3. Clean Code Principles (Robert C. Martin)

- **Meaningful Names:** Every function in `Interpreter.hs` and `Parser.hs` (e.g., `evalOp`, `matchPattern`, `findMatch`) explicitly states its intent.
- **Small Functions:** The evaluator is broken down into small, case-specific functions. No single function handles more than one logical step.
- **Single Responsibility:** `AST.hs` defines the structure, `Parser.hs` handles syntax, and `Interpreter.hs` handles logic. There is no mixing of concerns.
- **Don't Repeat Yourself (DRY):** Common logic like environment lookups and pattern matching are centralized.

## 4. How to Demo
To run the project, ensure you have `ghc` installed. Run the following command from the project root:

```bash
runghc -iFinal Final/Main.hs Final/demo.lp
```

## 5. References
Following the project requirements for professional citations:

1.  **Leijen, Daan.** (2001). *Parsec: Direct Style Monadic Parser Combinators for the Real World*. Department of Computer Science, Utrecht University. Accessed April 26, 2026.
2.  **Marlow, Simon (ed).** (2010). *Haskell 2010 Language Report*. Available at: https://www.haskell.org/onlinereport/haskell2010/. Accessed April 26, 2026.
3.  **Martin, Robert C.** (2008). *Clean Code: A Handbook of Agile Software Craftsmanship*. Prentice Hall. 
4.  **Meyer, Bertrand.** (1992). *Applying "Design by Contract"*. IEEE Computer, 25(10): 40-51. Accessed April 26, 2026.
5.  **Thompson, Simon.** (2011). *Haskell: The Craft of Functional Programming*. 3rd Edition. Addison-Wesley.

This will:
1. Parse the `demo.lp` file.
2. Execute the recursive `sum` and `length` functions.
3. Perform list filtering.
4. Print the final result and the state of the environment.

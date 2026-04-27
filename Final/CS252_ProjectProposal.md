# CS252 Project Proposal
## Design and Implementation of a Small Programming Language in Haskell

Krishna Mula &nbsp;&nbsp; Kushagra Bainsla &nbsp;&nbsp; Tejas Chakkarwar

---

## Project Overview

For our course project, we propose to design and implement a small programming language in Haskell. Our language will combine a simple imperative core with several higher-level language features that make it more expressive and interesting from a programming languages perspective.

The base language will include integers, booleans, variables, arithmetic expressions, conditionals, and sequencing. On top of this core, we plan to implement three major features:

- user-defined functions,
- lists, and
- pattern matching.

Our goal is to build a language that is small enough to finish within the quarter, but rich enough to demonstrate meaningful language design and implementation choices.

---

## Motivation

We chose this project because it fits the themes of the course very well: syntax, parsing, abstract syntax trees, semantics, and interpreters. Rather than only implementing a minimal WHILE-style language, we want to extend the language with features that appear in real programming languages and that make programs more expressive.

The three features we chose are meant to complement each other:

- Functions add abstraction and code reuse.
- Lists add a useful recursive data structure.
- Pattern matching provides a clean way to deconstruct lists and write recursive programs.

Together, these features make the language feel more complete and interesting than a basic interpreter alone.

---

## Planned Language Features

Our current design is a small language with the following components.

### Core Language

- integer and boolean literals
- variables
- arithmetic and comparison operators
- conditional expressions/statements
- sequencing of expressions/statements

### Major Features

1. User-defined functions with parameters and function calls
2. Lists, including list literals and basic recursive list structure
3. Pattern matching for case analysis over lists

---

## Implementation Plan

We plan to implement the following components in Haskell:

- a concrete syntax for the language
- a parser that converts programs into an abstract syntax tree (AST)
- AST data types representing expressions, functions, and patterns
- an interpreter that evaluates programs
- support for function calls and environments
- support for lists and pattern matching during evaluation
- a collection of test programs and example programs

If time permits, we may also add better error reporting or a lightweight static checker as a stretch goal.

---

## Sample of Proposed Language

Below is a small example of the kind of language we want to build:

```
fun length(xs) =
  match xs with
  | [] -> 0
  | h:t -> 1 + length(t)

fun sum(xs) =
  match xs with
  | [] -> 0
  | h:t -> h + sum(t)

x = [1,2,3,4];
sum(x)
```

This example shows all three of our main features: user-defined functions, lists, and pattern matching.

---

## Weekly Schedule

**Week 1:** Finalize language design, grammar, and division of responsibilities. Set up repository and project structure.

**Week 2:** Implement the abstract syntax tree and parser for the core language. Add support for literals, variables, arithmetic, and conditionals.

**Week 3:** Implement user-defined functions and function calls. Test recursive functions.

**Week 4:** Implement lists and list literals. Extend the interpreter to support list values.

**Week 5:** Implement pattern matching over lists. Integrate it with function evaluation and recursion.

**Week 6:** Testing, debugging, cleanup, and polishing. Prepare example programs, final report, and presentation materials.

---

## Expected Outcome

By the end of the project, we expect to have a working interpreter for a small language of our own design, implemented in Haskell, along with sample programs demonstrating functions, lists, and pattern matching. We also expect to better understand how language features interact at the level of syntax, semantics, and implementation.

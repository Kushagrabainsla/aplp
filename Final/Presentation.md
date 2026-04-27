# Prism Language - Project Presentation
**By: Krishna Mula, Kushagra Bainsla, Tejas Chakkarwar**

---

## 1. Project Goal: "Declarative Deconstruction"
- Design and implement a language that simplifies looking *inside* complex data.
- Move beyond basic arithmetic to support recursive deconstruction and advanced control flow.

---

## 2. Key Features
- **User-Defined Functions**: Recursive calls with local lexical scoping.
- **Recursive Lists**: Full support for `cons`, empty lists, and list literals.
- **Pattern Matching**: Robust `match` expression for elegant list processing.
- **Static Analysis**: Pre-execution check for unbound variables (Stretch Goal).

---

## 3. Language Syntax (Demo)
```haskell
fun sum(xs) =
  match xs with
  | [] -> 0
  | h:t -> h + sum(t)

x = [1, 2, 3, 4];
sum(x) // Result: 10
```

---

## 4. Implementation Details
- **Frontend**: Parsec (Monadic Parser Combinators).
- **Backend**: `State` and `ExceptT` monads for clean state management and error reporting.
- **Scoping**: Lexical environment preservation during function calls.

---

## 5. Clean Code & Design Rationale
- **Small Functions**: Every evaluation step is delegated to specific helpers.
- **Descriptive Errors**: "Static Analysis Error" vs "Runtime Error" vs "Parse Error".
- **Sound Reasoning**: Why use a native Haskell list? Why make `match` an expression?

---

## 6. Outcomes
- Successfully implemented all proposed features.
- Achieved "Stretch Goal" of implementing a static name-resolution pass.
- Produced a robust test suite covering recursion, list ops, and edge cases.

---

## 7. Demo
*Run:* `runghc -iFinal Final/Main.hs Final/demo.lp`

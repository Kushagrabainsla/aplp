# Lab 17: Part 5 (Extra Credit) Technical Deep-Dive

## 1. Objective
The goal was to implement conditional logic (`if` expressions) and Boolean support within our Scheme-to-EVM compiler and Virtual Machine. This requires shifting from a simple linear execution model to a branching model using jumps.

## 2. Low-Level Mechanics

### Boolean Representation
- **Scheme**: `#t` (true) and `#f` (false).
- **Bytecode**: Mapped to `1` and `0` respectively.
- **Why?**: The EVM `JUMPI` instruction checks if a value is non-zero. By mapping booleans to 1 and 0, we can directly use the results of logical expressions to drive jump decisions.

### Opcode Implementation
- **JUMP (0x56)**: An unconditional jump. It pops the destination address from the stack and sets the `pc` (Program Counter) to that value.
- **JUMPI (0x57)**: A conditional jump. It pops the destination and a condition. It only updates the `pc` if the condition is non-zero.
- **PC Offset Correction**: In the VM, we set `vm.pc = dest - 1`. We subtract 1 because the VM's main `while` loop increments the `pc` at the end of every cycle. Subtracting 1 ensures that the next cycle starts exactly at the intended instruction.

## 3. Compiler Strategy: Label Backfilling

The biggest challenge in a single-pass compiler is "forward referencing." When the compiler sees an `if`, it needs to generate a jump to the "then" branch, but it doesn't know where that branch starts yet.

### The Algorithm:
1. **Compile Condition**: Generate bytecode for the test (e.g., `#t`).
2. **Push Placeholder**: Generate a `PUSH1` opcode followed by a dummy byte `0`.
3. **Emit JUMPI**: Generate the conditional jump instruction.
4. **Compile Else Branch**: Generate bytecode for the "else" part.
5. **Emit JUMP**: Generate an unconditional jump to skip the "then" branch after the "else" branch finishes.
6. **Capture Offset**: Record the current `this.offset` (this is our "Then Label").
7. **Backfill**: Use `this.bytecode.writeUInt8(thenLabel, placeholderIndex)` to replace the dummy `0` with the actual address.
8. **Compile Then Branch**: Generate bytecode for the "then" part.

## 4. Verification (The Demo)
Using `cond.scm`, we can prove the logic is sound:

```scheme
(println #t)                        ; Outputs 1
(if #t (println 3) (println 4))     ; Outputs 3 (Jumps to Then)
(if #f (println 3) (println 4))     ; Outputs 4 (Executes Else, Jumps to End)

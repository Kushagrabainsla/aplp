# Homework 3 run notes

## Issue in HW3.hs

The parser in HW3.hs only accepts the token "end" to close blocks. The provided sample .imp programs use "endif" and "endwhile" instead of "end". This causes parse errors like:

- unexpected reserved word if
- unexpected reserved word while

## Fix in HW3-latest.hs

The updated file accepts both forms:

- if ... then ... else ... end
- if ... then ... else ... endif
- while ... do ... end
- while ... do ... endwhile

This is implemented by adding helper parsers that accept either keyword, and by adding "endif" and "endwhile" to the reserved words list.

## Change from HW3.hs to HW3-latest.hs

HW3-latest.hs is the updated version that expands the parser to accept "endif" and "endwhile" in addition to "end". No interpreter semantics were changed; only the parser accepts more block terminators.

## Test commands

runhaskell haskell/homework3/HW3.hs content/hw__hw3__test.imp
runhaskell haskell/homework3/HW3-latest.hs content/hw__hw3__test.imp

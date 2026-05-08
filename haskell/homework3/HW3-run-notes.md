# Homework 3 run notes

## Issue in HW3-1.hs

The parser in HW3-1.hs only accepts the token "end" to close blocks. The provided sample .imp programs use "endif" and "endwhile" instead of "end". This causes parse errors like:

- unexpected reserved word if
- unexpected reserved word while

## Fix in HW3-latest.hs

The updated file accepts both forms:

- if ... then ... else ... end
- if ... then ... else ... endif
- while ... do ... end
- while ... do ... endwhile

This is implemented by adding helper parsers that accept either keyword, and by adding "endif" and "endwhile" to the reserved words list.

## Test commands

runhaskell haskell/homework3/HW3-1.hs content/hw__hw3__test.imp
runhaskell haskell/homework3/HW3-latest.hs content/hw__hw3__test.imp

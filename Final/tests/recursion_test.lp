// recursion_test.lp
// Tests deep recursion and mathematical functions

fun fact(n) =
  if n == 0 then 1 else n * fact(n - 1)

fun fib(n) =
  if n == 0 then 0 
  else if n == 1 then 1 
  else fib(n - 1) + fib(n - 2)

f10 = fact(10);
fib7 = fib(7);

f10

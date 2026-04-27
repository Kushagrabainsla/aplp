// error_cases.lp
// Demonstrates graceful error handling

fun safe_div(a, b) =
  if b == 0 then 0 else a / b

// This will trigger a runtime error if run directly
// x = 10 / 0; 

// This demonstrates safe handling
result = safe_div(10, 0);

// Unbound variable example (will fail if uncommented)
// z = unbound_var + 1;

result

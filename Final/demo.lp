// CS252 Final Project Demo
// Language: Prism (formerly LPC)

// 1. User-defined functions & Recursion
fun length(xs) =
  match xs with
  | [] -> 0
  | h:t -> 1 + length(t)

fun sum(xs) =
  match xs with
  | [] -> 0
  | h:t -> h + sum(t)

// 2. Advanced Pattern Matching
fun filter_even(xs) =
  match xs with
  | [] -> []
  | h:t -> 
      if (h / 2) * 2 == h 
      then h : filter_even(t) 
      else filter_even(t)

// 3. Nested Lists
fun flatten(xs) =
  match xs with
  | [] -> []
  | h:t -> h // This is a simplified demo; actual flatten would need more logic

// 4. Main Execution
my_list = [1, 2, 3, 4, 5, 6];
s = sum(my_list);
l = length(my_list);
evens = filter_even(my_list);

// Final expression to return
s + l

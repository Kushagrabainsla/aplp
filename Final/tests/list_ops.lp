// list_ops.lp
// Tests list concatenation, mapping, and nested patterns

fun reverse_acc(xs, acc) =
  match xs with
  | [] -> acc
  | h:t -> reverse_acc(t, h : acc)

fun reverse(xs) = reverse_acc(xs, [])

fun map_inc(xs) =
  match xs with
  | [] -> []
  | h:t -> (h + 1) : map_inc(t)

my_list = [1, 2, 3];
rev = reverse(my_list);
inc = map_inc(my_list);

rev

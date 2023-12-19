Execute this code via `clj -M -m day16 <filename>`.

### Observations

1. Implementing worklist/set with recursive functions (and without `loop` and
   `recur`) led to stack overflow issues; see `track-beam1` function in the
   code history of the solution.  Does Clojure optimize tail recursive functions? 
   If it does, then an easy way to check if a tail recursive function is
   optimized will be helpful.
2. The solution provided an off-by-one answer for the test data in part 2 but
   it gave the correct answer for the data.  Not sure what is wrong.

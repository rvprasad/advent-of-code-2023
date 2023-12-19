Execute this code via `clj -M -m day11 <filename>`.

### Observations

1. Implementing worklist/set with recursion and without `loop` and `recur` led 
   to stack overflow issues; see `track-beam1` function in the code history of
   the solution.  No easy way to figure out 

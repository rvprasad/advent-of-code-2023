Execute this code via `elixir day19.exs <filename>`.

### Observations

1. As expected, the lack of static type checking bites as the number of functions
   and levels of abstractions increases.
2. Compared to coding in Rust or Ocaml, the IDE is not cluttered by type errors as
   the program is being written.  This lack of clutter helped me focus on the
   program logic.  Once the program is complete, executing it surfaces type
   errors which often were easy to fix.  So, a few thoughts.

   1. Not enabling the type checker during coding may help improve focus on
      program logic and getting it right.
   2. May be, still enabling code formatting; specifically, while coding with white
      space-sensitive languages.
   3. Without enabling the type checker, the language server may not be able to
      provide useful hints and break focus.  Seems like a catch-22 with point 1
      above.
   4. A way out may be to have _typeless_ view in IDEs.  Like we have views/lenses
      that add display inferred type hints, we could have a view/lense that
      suppresses the display of existing types and type errors.

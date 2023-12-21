Execute via `dune exec ./day17.exe <filename>`

### Observations

1. To bootstrap a single file ocaml project,
   1. Execute `dune init project day17` in the root folder.
   2. In the newly created `day17` folder, do the following.
      1. Execute `dune init exec day17`.
      2. Delete `bin`, `lib`, and `test` folders.
      3. Delete `day17.opam` file.
2. Use `utop` instead of `ocaml` for a nice REPL experience.

Execute this code via `cargo run <filename>`.

### Observations

1. Bootstrapping a single file program was a breeze.
2. Compiler error messages and suggestions are awesome.
3. Using `cargo watch` for watched builds was nice.
4. Ownership was a bit confusing; specifically, the special treatment of primitive types.
5. Using standard libraries was a bit unintuitive and clunky as some types implemented the `Copy` trait while some did not and this did not play well with functional programming idioms.

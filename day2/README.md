Execute this code via `dune exec ./day2.exe <filename> <red count> <green count> <blue count>`.

### Observations

1. Opening `Stdio` module at top-level conflicted with use of the `In_channel`
   module.
2. If `Core` module was opened at the top-level along with dependency on
   `core_unix` library, the compiler kept flagging `String.split_on_char`
   should be `String.split_on_chars`.  Strangely, `Core` library is opened at
   the top-level in most examples in [Real-world Ocaml](https://www.cambridge.org/core/books/real-world-ocaml-functional-programming-for-the-masses/052E4BCCB09D56A0FE875DD81B1ED571) book.
3. https://v2.ocaml.org/releases/5.1/api/index.html was good.  Capturing
   interfering behavior can improve it, e.g., bullet 1 above.
4. Instructions to bootstrap single file programs can be simpler and easier.

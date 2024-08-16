Essentials of Compilation
===

An Incremental Approach in ~Racket~ Haskell

Working through [this book][essentials-of-compilation] using a [programming
language with no semantics][snail] in Haskell. Programs are written in Snail,
converted into the source language's AST, and then interpreted or compiled.

## Things to consider changing

- Using `logging-effect` is kind of annoying? Maybe I am doing it wrong. It
  might be best to just use `traceM` if I am actually going to write a book.

[essentials-of-compilation]: https://mitpress.mit.edu/9780262047760/essentials-of-compilation/
[snail]: https://github.com/chiroptical/snail


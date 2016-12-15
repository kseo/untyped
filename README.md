untyped
=======
An implementation of untyped lambda calculus in Haskell

# Overview

`untyped` is an implementation of untyped lambda calculus presented by Benjamin C. Pierce in his [Types and Programming Languages][tapl]. I transliterated the original source code written in OCaml into Haskell except for the parser which I rewrote in [Parsec][parsec].

# Syntax

* Variables: x
* Applications: x x
* Lambda abstractions: \x.x

# Implementation Details

* The representation of a variable is a number - its De Bruijn index
* Evaluation performs substitution
* Pretty printer removes redundant parenthesis

# REPL

`untypedi` is a REPL where you can input a lambda calculus term.

```
% (\x.x) (\x.x)
\x.x
% (\x.y)
Invalid lambda caluclus: (\x.y)
```

[parsec]: https://hackage.haskell.org/package/parsec
[tapl]: https://www.google.co.kr/search?q=tapl&oq=tapl&aqs=chrome..69i57j69i60l3j0l2.776j0j7&sourceid=chrome&ie=UTF-8#q=tapl+benjamin

# haskell-src-exts-qq: A quasiquoter for haskell-src-exts

[![Build Status](https://travis-ci.org/mboes/haskell-src-exts-qq.svg?branch=master)](https://travis-ci.org/mboes/haskell-src-exts-qq)
[![Hackage](https://img.shields.io/hackage/v/haskell-src-exts-qq.svg)](https://hackage.haskell.org/package/haskell-src-exts-qq)

Allows one to write programs that generate Haskell programs much more
concisely and legibly. This package supports:

* Antiquotations, denoted by stealing the splice syntax of Template
Haskell, for example:
```Haskell
[hs| $x ++ $(Hs.strE "bar") |]
```
Splices may not nested.

* Antiquoting pattern variables in patterns, using double parentheses.
For instance:
```Haskell
let x = Hs.name "n"
in [hs| \ ((x)) -> $(Hs.Var (Hs.UnQual x)) + 1 |]
```

* Antiquoting bound names. Names that are antiquoted appear surrounded
by double underscores. For instance:

```Haskell
let f = "incr"
    fE = Hs.Var $ Hs.UnQual $ Hs.name f
in [hs| let __f__ x = x + 1 in $fE 10 |]
```

We need three different syntaxes for antiquotations, because we do not
extend the haskell-src-exts parser in any way and the Template Haskell
splicing syntax is only available in expression contexts.

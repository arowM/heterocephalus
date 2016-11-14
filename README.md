[![Build Status](https://travis-ci.org/arowM/heterocephalus.svg?branch=master)](https://travis-ci.org/arowM/heterocephalus)
[![Hackage](https://img.shields.io/hackage/v/heterocephalus.svg)](https://hackage.haskell.org/package/heterocephalus)
[![Stackage LTS](http://stackage.org/package/heterocephalus/badge/lts)](http://stackage.org/lts/package/heterocephalus)
[![Stackage Nightly](http://stackage.org/package/heterocephalus/badge/nightly)](http://stackage.org/nightly/package/heterocephalus)

![hetero-mini](https://cloud.githubusercontent.com/assets/1481749/20267445/2a9da33e-aabe-11e6-8aa7-88e36f0a8d5d.jpg)

# Heterocephalus template engine

A flexible and type safe template engine for Haskell.

Currently, this module uses lots of codes from [`hamlet`](http://hackage.haskell.org/package/shakespeare-2.0.11/docs/Text-Hamlet.html).

## Who should use this?

If you are planning to use Haskell with recent web front-end tools like gulp, webpack, npm,..., this library helps you!

As you know, there are many Haskell template engines today.
The [shakespearen template](http://hackage.haskell.org/package/shakespeare) is great because it checks template variables on compile time and no run time error caused by template file.
But how we can embed variables on Haskell backend to a file generated with recent complicated front-end flow?
The shakespearen template does not resolve the problem because they force us to use their original way to write Html, Css, JavaScript, and so on.
Though we can use `Text.Shakespeare.Text` module to embed template variable to any file, the module lacks control statements like `forall` and `if`.

A [`haiji`](https://hackage.haskell.org/package/haiji) was only choice for the use case, but it [takes too long compile time](https://github.com/blueimpact/kucipong/pull/7) when we use `stack` (ghc >= 7.10).

This is the motivation of this module.
The Heterocephalus template engine has feature like `haiji` but it does not take too long compile time.

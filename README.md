[![Build Status](https://travis-ci.org/arowM/heterocephalus.svg?branch=master)](https://travis-ci.org/arowM/heterocephalus)
[![Hackage](https://img.shields.io/hackage/v/heterocephalus.svg)](https://hackage.haskell.org/package/heterocephalus)
[![Stackage LTS](http://stackage.org/package/heterocephalus/badge/lts)](http://stackage.org/lts/package/heterocephalus)
[![Stackage Nightly](http://stackage.org/package/heterocephalus/badge/nightly)](http://stackage.org/nightly/package/heterocephalus)

![hetero-mini](https://cloud.githubusercontent.com/assets/1481749/20267445/2a9da33e-aabe-11e6-8aa7-88e36f0a8d5d.jpg)

# Heterocephalus template engine

A type safe template engine for collaborating with front end development tools.

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

## Features

Here are the features of this module.

* __DO__ ensure that all interpolated variables are in scope

* __DO__ ensure that all interpolated variables have proper types for the template

* __DO__ expand the template literal on compile time

* __DO__ provide you the way to `forall` and `if` in the template

    `Text.Shakespeare.Text.text` only have a feature to embed variables but __DOES NOT__ have way to do this.

* __DO NOT__ enforce template to obey the peculiar rule

    Shakespeare templates basically enforces you the indent base peculiar style.
    Though `Text.Shakespeare.Text.text` does not enforce you to do so, it has no `forall` and `if` statement.

    This fact makes it impossible you to use Shakespeare with another template engine such as `pug` in front end side.
    It is not suitable for recent rich front end tools.

* __DO NOT__ take too long compile time

    `haiji` is another awesome library it has much of features and compile time template expanding, but it takes too long compile time when used with stack (ghc >= 7.10).

* __DO NOT__ provide convenient control statements

    There are rich control statements such as importing external file in other Haskell template engines, but this module does not need them because this is supposed to be used with rich front end side template engine.

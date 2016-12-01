[![Build Status](https://travis-ci.org/arowM/heterocephalus.svg?branch=master)](https://travis-ci.org/arowM/heterocephalus)
[![Hackage](https://img.shields.io/hackage/v/heterocephalus.svg)](https://hackage.haskell.org/package/heterocephalus)
[![Stackage LTS](http://stackage.org/package/heterocephalus/badge/lts)](http://stackage.org/lts/package/heterocephalus)
[![Stackage Nightly](http://stackage.org/package/heterocephalus/badge/nightly)](http://stackage.org/nightly/package/heterocephalus)

![hetero-mini](https://cloud.githubusercontent.com/assets/1481749/20267445/2a9da33e-aabe-11e6-8aa7-88e36f0a8d5d.jpg)

# Heterocephalus template engine

A type safe template engine for collaborating with front end development tools.

Any PR even if about documents are welcome because main author of this library is not an English native.

* [Who should use this?](#who-should-use-this)
* [Features](#features)
* [Usage](#usage)
* [Checking behaviours in `ghci`](#checking-behaviours-in-ghci)
* [Syntax](#syntax)

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

Here are the main features of this module.

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

## Usage

You can compile external template file with four functions bellow.
As the function name represents, each function provide following features.

* `compileTextFile`: A basic function that embeds variables without escaping and without default values.
* `compileTextFileWithDefault`: Same as `compileTextFile` but you can set default template values.
* `compileHtmlFile`: Same as `compileTextFile` but all embeded variables are escaped for html.
* `compileHtmlFileWithDefault`: Same as `compileHtmlFile` but you can set default template values.

For more details, see [latest haddock document](https://www.stackage.org/haddock/nightly/heterocephalus/Text-Heterocephalus.html).

## Checking behaviours in `ghci`

To check the behaviour, you can test in `ghci` as follows. Note that `compileText` and `compileHtml` are used for checking syntaxes.

```haskell
$ stack install heterocephalus  # Only first time
$ stack repl --no-build --no-load
Prelude> :m Text.Heterocephalus Text.Blaze.Renderer.String
Prelude> :set -XTemplateHaskell -XQuasiQuotes
Prelude> let a = 34; b = "<script>"; in renderMarkup [compileText|foo #{a} #{b}|]
"foo 34 <script>"
Prelude> let a = 34; b = "<script>"; in renderMarkup [compileHtml|foo #{a} #{b}|]
"foo 34 &lt;script&gt;"
```

## Syntax

This module provide two major syntaxes for template file; variable interpolation and control statements.

### Variable interpolation

You can embed Haskell variable to the template file.
As follows, a statement surrounded by `#{` and `}` is expanded on compile time, and injected on run time.

#### Basic usage

All of followings are correct syntax.
Assumes that you have already declared the `var` variable in Haskell program and it is in scope.

```text
#{ var }
#{var}
#{  var}
#{var  }
#{ var  }
```

The variable we can embed must be an instance of `Text.Blaze.ToMarkup`.

#### Advanced usage

You can use functions and data constructors as well.

```text
#{ even num }
#{ num + 3 }
#{ take 3 str }
#{ maybe "" id (Just b) }
```

### Control statements

Only two type of control statements are provided.
Rich control statements are not required in our use case of using with front end tools.

#### Forall

Sample.

```
%{ forall x <- xs }
#{x}
%{ endforall }

%{ forall (k,v) <- kvs }
#{k}: #{v}
%{ endforall }
```

#### If

Sample.

```
%{ if even num }
#{num} is even number.
%{ else }
#{num} is odd number.
%{ endif }
```

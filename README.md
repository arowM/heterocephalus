[![test](https://github.com/arowM/heterocephalus/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/heterocephalus/actions/workflows/test.yaml)
[![Hackage](https://img.shields.io/hackage/v/heterocephalus.svg)](https://hackage.haskell.org/package/heterocephalus)
[![Stackage LTS](http://stackage.org/package/heterocephalus/badge/lts)](http://stackage.org/lts/package/heterocephalus)
[![Stackage Nightly](http://stackage.org/package/heterocephalus/badge/nightly)](http://stackage.org/nightly/package/heterocephalus)

![hetero-mini](https://cloud.githubusercontent.com/assets/1481749/20267445/2a9da33e-aabe-11e6-8aa7-88e36f0a8d5d.jpg)

# Heterocephalus template engine

A type-safe template engine for working with popular front end development tools.

Any PRs are welcome, even for documentation fixes.  (The main author of this library is not an English native.)

* [Who should use this?](#who-should-use-this)
* [Features](#features)
* [Usage](#usage)
* [Checking behaviours in `ghci`](#checking-behaviours-in-ghci)
* [Syntax](#syntax)
* [Why "heterocephalus"?](#why-heterocephalus)

## Who should use this?

If you are planning to use Haskell with recent web front-end tools like gulp,
webpack, npm, etc, then this library can help you!

There are many Haskell template engines today.
[Shakespeare](http://hackage.haskell.org/package/shakespeare) is great because
it checks template variables at compile time.  Using Shakespeare, it's not
possible for the template file to cause a runtime-error.

Shakespeare provides its own original ways of writing HTML
([Hamlet](https://hackage.haskell.org/package/shakespeare/docs/Text-Hamlet.html)),
CSS
([Cassius](https://hackage.haskell.org/package/shakespeare/docs/Text-Cassius.html)
/
[Lucius](https://hackage.haskell.org/package/shakespeare/docs/Text-Lucius.html)),
and JavaScript
([Julius](https://hackage.haskell.org/package/shakespeare-2.0.11.2/docs/Text-Julius.html)).
If you use these original markup languages, it is possible to use control
statements like `forall` (for looping), `if` (for conditionals), and `case`
(for case-splitting).

However, if you're using any other markup language (like
[pug](https://pugjs.org), [slim](http://slim-lang.com/),
[haml](http://haml.info/), normal HTML, normal CSS, etc), Shakespeare only
provides you with the
[Text.Shakespeare.Text](https://hackage.haskell.org/package/shakespeare/docs/Text-Shakespeare-Text.html)
module.  This gives you variable interpolation, but no control statements like
`forall`, `if`, or `case`.

[`Haiji`](https://hackage.haskell.org/package/haiji) is another interesting
library.  It has all the features we require, but its templates take a very
[long time to compile](https://github.com/blueimpact/kucipong/pull/7) with
GHC >= 7.10.

Heterocephalus fills this missing niche. It gives you variable interpolation
along with control statements that can be used with any markup language.  Its
compile times are reasonable.

## Features

Here are the main features of this module.

* __DO__ ensure that all interpolated variables are in scope

* __DO__ ensure that all interpolated variables have proper types for the template

* __DO__ expand the template literal on compile time

* __DO__ provide a way to use `forall`, `if`, and `case` statments in the template

    `Text.Shakespeare.Text.text` has a way to do variable interpolation, but no
    way to use these types of control statements.

* __DO NOT__ enforce that templates obey a peculiar syntax

    Shakespeare templates make you use their original style (Hamlet, Cassius,
    Lucius, Julius, etc).  The
    [`Text.Shakespeare.Text.text`](https://hackage.haskell.org/package/shakespeare/docs/Text-Shakespeare-Text.html#v:text)
    function does not require you to use any particular style, but it does not
    have control statements like `forall`, `if` and `case`.

    This makes it impossible to use Shakespeare with another template engine
    such as `pug` in front end side.  It is not suitable for recent rich front
    end tools.

* __DO NOT__ have a long compile time

    `haiji` is another awesome template library. It has many of our required
    features, but it takes too long to compile when used with ghc >= 7.10.

* __DO NOT__ provide unneeded control statements

    Other template engines like [EDE](https://hackage.haskell.org/package/ede)
    provide rich control statements like importing external files.
    Heterocephalus does not provide control statements like this because it is
    meant to be used with a rich front-end template engine (like pug, slim,
    etc).

## Usage

You can compile external template files with the following four functions:

* `compileTextFile`: A basic function that embeds variables without escaping and without default values.
* `compileTextFileWithDefault`: Same as `compileTextFile` but you can set default template values.
* `compileHtmlFile`: Same as `compileTextFile` but all embeded variables are escaped for html.
* `compileHtmlFileWithDefault`: Same as `compileHtmlFile` but you can set default template values.

For more details, see the [latest haddock
document](https://www.stackage.org/haddock/nightly/heterocephalus/Text-Heterocephalus.html).

## Checking behaviours in `ghci`

To check the behaviour, you can test in `ghci` as follows. Note that
`compileText` and `compileHtml` are used for checking syntaxes.

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

The
[Text.Heterocephalus](https://www.stackage.org/haddock/nightly/heterocephalus/Text-Heterocephalus.html)
module provides two major features for use in template files: variable interpolation
and control statements.

### Variable interpolation

A Haskell variable can be embedded in the template file with the `#{foo}`
syntax.  The value of the variable will be injected in at run time.

#### Basic usage

All of following are correct (this assumes that you have already declared the
`var` variable in your Haskell program and it is in scope):

```text
#{ var }
#{var}
#{  var}
#{var  }
#{ var  }
```

The variable must be an instance of
[`Text.Blaze.ToMarkup`](https://hackage.haskell.org/package/blaze-markup/docs/Text-Blaze.html#t:ToMarkup).

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

#### Forall

```
%{ forall x <- xs }
#{x}
%{ endforall }

%{ forall (k,v) <- kvs }
#{k}: #{v}
%{ endforall }
```

#### If

```
%{ if even num }
#{num} is even number.
%{ else }
#{num} is odd number.
%{ endif }
```

```
%{ if (num < 30) }
#{ num } is less than 30.
%{ elseif (num <= 60) }
#{ num } is between 30 and 60.
%{ else }
#{ num } is over 60.
%{ endif }
```

#### Case

```
%{ case maybeNum }
%{ of Just 3 }
num is 3.
%{ of Just num }
num is not 3, but #{num}.
%{ of Nothing }
num is not anything.
%{ endcase }
```

```
%{ case nums }
%{ of (:) n _ }
first num is #{n}.
%{ of [] }
no nums.
%{ endcase }
```

#### Why we do not provide `maybe` and `with`?

TODO

Discussions about this topic is on [issue #9](https://github.com/arowM/heterocephalus/issues/9).

## Why "heterocephalus"?

"Heterocephalus" is the scientific name of the [naked mole-rat](https://en.wikipedia.org/wiki/Naked_mole-rat).

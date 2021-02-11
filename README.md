# `text-ascii`

## What is this thing?

A library for handling ASCII text.

## What are the goals of this project?

### Totality by default

Partial functions (and type classes which provide them) will not be included:
everything is total. When we include anything unsafe, it will be explicitly
firewalled into its own module, behind a newtype.

### Compatibility with the [`text`](http://hackage.haskell.org/package/text) API

We have (without conflicting with totality) a goal to match the API of the
`text` package exactly. If you know how to use `text`, you know how to use this
package too.

### No boolean blindness

[Boolean blindness](http://dev.stephendiehl.com/hask/#boolean-blindness) is not
a good thing, for all the reasons listed in the link. Whenever possible, we'll
try and give more useful information than a `Bool`.

### Discoverability, documentation and user-friendliness

In addition to documenting everything with Haddocks, we have over 100 doctests,
which provide _executable_ examples of how the API can be used, and how it will
behave. We aim to clarify _every_ corner case left by the documentation of
`text`, and care strongly about making the API easy to follow, learn and
understand.

### Correctness

We currently use doctests, but plan to add support for more testing. No such
thing as too much!

### Low dependencies

As far as possible, we aim to depend on [GHC boot packages](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/libraries/version-history) only. When we
introduce more dependencies, we do it only when we have to. This way, we ensure
this package builds quickly and doesn't 'lag' more than necessary due to GHC
version changes.

## What's with all the cat stuff?

[I am a Haskell catboy.](https://twitter.com/KozRoss)

## What does this run on?

Currently, our CI checks the following versions of GHC:

* 8.6.5
* 8.8.4
* 8.10.3

We check on the following platforms:

* Windows
* Linux
* MacOS

## What can I do with this?

The project is licensed Apache 2.0 (SPDX code
[`Apache-2.0`](https://spdx.org/licenses/Apache-2.0.html)). For more details,
please see the `LICENSE.md` file.

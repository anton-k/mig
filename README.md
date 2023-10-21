# Mig - library to write composable and lightweight servers

[![build](https://github.com/anton-k/mig/actions/workflows/compile-and-test.yml/badge.svg)](https://github.com/anton-k/mig/actions/workflows/compile-and-test.yml)

The Mig is a library to build lightweight composable servers.
The main strength is ability to build servers from parts
and flexible DSL which features only small amount of functions.

I like [scotty](https://hackage.haskell.org/package/scotty) for being very 
simple and [servant](https://hackage.haskell.org/package/servant-server) for being composable, type-safe 
and how functions are used as handlers which provides decoupling of Web-handlers
from application logic.
But sometimes scotty feels too imperative and lacks servant's composability.
And servant with type-level magic and huge errors can feel to complicated.
So I wanted to create something in the middle. Something composable and simple 
at the same time.
The name `mig` (pronounced as meeg) is a russian word for "instant moment".

* Quick start guide: [tutorial](https://anton-k.github.io/mig/)
* Summary of the main functions: [refs](https://anton-k.github.io/mig/09-reference.html)
* How to contribute: [guide](https://anton-k.github.io/mig/10-how-to-contribute.html)

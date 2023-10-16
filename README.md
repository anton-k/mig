# Mig - library to write composable and lightweight servers

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

* For quick start guide see [tutorial](https://anton-k.github.io/mig/)
* For summary of the main functions see [docs](https://anton-k.github.io/mig/08-reference.html)

# [Mig](https://anton-k.github.io/mig/) - library to write composable and lightweight servers

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

### Structure of the repo

An overview of the mig repo:

* `mig` - core library. It defines DSL and low-level represenatation for API and server as a function

* `mig-wai` - conversion of mig servers to WAI applications

* `mig-extra` - extra utils for core library

* `mig-swagger-ui` - swagger servers for mig servers. Offers nice in the browser UI to test HTTP REST applications

* `mig-server` - mig servers based on warp with batteries included

* `mig-rio` - binding to rio library. It comtains instance of the `HasServer` class for RIO type.

* `examples` - several examples of servers and clients. Examples can be run with commands in the [`Makefile`](https://github.com/anton-k/mig/blob/main/examples/mig-example-apps/Makefile) of their subdirectory.

* `docs` - tutorial for the library and reference of the main functions. It is build with `mdbook` and deployed on github pages.

See [`Makefile`](https://github.com/anton-k/mig/blob/main/Makefile) for main commands to work with repo in dev mode.

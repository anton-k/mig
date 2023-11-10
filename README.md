# [Mig](https://anton-k.github.io/mig/) - library to write composable and lightweight servers

[![build](https://github.com/anton-k/mig/actions/workflows/compile-and-test.yml/badge.svg)](https://github.com/anton-k/mig/actions/workflows/compile-and-test.yml)
[![Version on Hackage](https://img.shields.io/hackage/v/mig-server.svg)](https://hackage.haskell.org/package/mig-server)
[![docs - tutorial](https://img.shields.io/badge/docs-Tutorial-2ea44f)](https://anton-k.github.io/mig/)
[![docs - examples](https://img.shields.io/badge/docs-Examples-2ea44f)](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps#mig-example-apps)

The Mig is a library to build lightweight composable servers.
The main strength is ability to build servers from parts
and flexible and type-safe DSL which features only small amount of functions.
The name `mig` (pronounced as meeg) is a russian word for "instant moment".

### Hello world server example

With the library [`mig-server`](https://hackage.haskell.org/package/mig-server) installed we can define 
a simple server with two routes:

```haskell
import Mig.Json.IO

-- | Starts server on port 8085.
main :: IO ()
main = runServer 8085 (withSwagger def server)

-- | The server definition
server :: Server IO
server = 
  "api/v1" /. 
    [ "hello" /. hello
    , "bye" /. bye
    ]

-- | The handler definition as a function
hello :: Get (Resp Text)
hello = pure $ ok "Hello World"

-- | The handler definition as a function with a query parameter to ask for user name
bye :: Query "user" Text -> Get (Resp Text)
bye (Query name) = pure $ ok ("Goodbye " <> name)
```

We can test the server with curl or with swagger-ui if it's run localy on the url [http://localhost:8085/swagger-ui](http://localhost:8085/swagger-ui).
For more examples see the [directory](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps). 

### Comparison to Scotty and Servant

I like [scotty](https://hackage.haskell.org/package/scotty) for being very 
simple and [servant](https://hackage.haskell.org/package/servant-server) for being composable, type-safe 
and how functions are used as handlers which provides decoupling of Web-handlers
from application logic.
But sometimes scotty feels too imperative and lacks servant's composability.
And servant with type-level magic and huge errors can feel to complicated.
So I wanted to create something in the middle. Something composable and simple 
at the same time.


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

### Tutorial and other links

* Quick start guide: [tutorial](https://anton-k.github.io/mig/)
* Summary of the main functions: [refs](https://anton-k.github.io/mig/09-reference.html)
* How to contribute: [guide](https://anton-k.github.io/mig/10-how-to-contribute.html)
* Examples: [examples directory](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps)


# Mig by example

Mig is a lightweight and easy to use library to build servers in Haskell.
It is sort of servant for Simple/Boring Haskell.
This book is an example driven guide to the library.

The main features are:

* lightweight library

* easy to use. It has simple design on purpose

* it defines no custom server monads. I promise you

* expressive DSL to compose servers

* type-safe route handlers and conversions

* handlers are encoded with generic haskell functions

* built on top of WAI and warp server libraries.

* provides Swagger to your server with one-line of code

* relies on standard classes to compose servers. The server is a monoid 

Example of hello world server:


```haskell
import Mig.Json.IO

-- | Starts server on port 8085.
main :: IO ()
main = runServer 8085 server

-- | The server definition
server :: Server IO
server = "api/v1/hello" /. hello

-- | The handler definition as a function
hello :: Get (Resp Text)
hello = Send $ pure $ ok "Hello World"
```


## How to install library

We can install it from hackage. 
we need to use the library [mig-server](https://hackage.haskell.org/package/mig-server)

With cabal we can install it from Hackage:

```
cabal install mig-server --lib
```

With stack we can link to the repo in extra-deps

```
TODO: example here
```

## Sturcuture of the library

There are several libraries:

- `mig` - core library which defines DSL to build servers with API-schemas and functions to render it to low-level representation. 
- `mig-extra` - extra addons to the core library
- `mig-server` - mig core with batteries and functions to run servers on top of warp.
- `mig-client` - HTTP-clients from the server code
- `mig-wai` - connvert mig servers to WAI-applications
- `mig-swagger-ui` - serve swagger for you app.

## Source code for examples

We are going to learn how the mig works by examples.
You can run the examples from the tutorial. Here is the [code](https://github.com/anton-k/mig/tree/main/examples/mig-example-apps#readme) that we are going to study.
Look at the Makefile for commands on how to build and run the examples.

Let's dive in to the first example.

## Comparing to other libraries

Why to use mig if there are other cool libraries?
To me mig lies in the middle ground between servant and scotty.
It's as simple as  scotty and does not go to fancy type road as servant.
But it is akin to servant in usage of type-safe conversions and type-level safety.

### servant

The mig uses the same ideas of type-safe handlers which a re based on generic Haskell functions.
The main difference is that in servant th whole server is described as type. 
Which leads to type-safety and ability to derive API sche, from the type.

But downside of it is fancy big types and whery advanced concepts that user needs to know
in order to use the library. Also one drawback to me is when things go wrong and you get
several pages long error messages. If your server is really big it can be very hard to spot
the origin of the error as type mismatch is going to be with the whole type which describes 
the full server.

The mig borrows idea of type-safe functions to represent route handlers. 
But types represent only individual handlers. It does not describe the full server.
But we have typesafety on the level of the single route. And error messages are going
to be localised and dedeicated to a single route. 

Using type-level description of the routes provide the same benefits as in serbvant case:

* safe type check of the conversions of low level request and response elements
* usage of generic haskell functions as handlers
* declarative design of the servers

In the mig API is a value that is derived from the server at run-time. 
It allows us to build clients and OpenApi swagger too.

To me servant is more demanding and complicated solution. I'd like to use 
something more simple.

### scotty

The scotty is also in domain of simple, easy to use solutions. 
so why did I wrote mig and havn't used the scotty instead?
Scotty features more imperative approach where you write handlert as 
expression for Scotty library monad. But it does not looks so well as in servant's case to me.
It is harder to assemble servers from parts. and I really like the idea of type-safe
convertions of various parts of request and response. 


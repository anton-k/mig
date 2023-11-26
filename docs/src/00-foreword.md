# Mig by example

Mig is a lightweight and easy to use library to build HTTP servers and clients in Haskell.
It is kind of servant for Simple/Boring Haskell.
This book is an example driven guide to the library.
The name `mig` (pronounced as meeg) is a russian word for "instant moment".

The main features of the mig library are:

* lightweight library
* easy to use. It has simple design on purpose
* expressive DSL to compose servers
* type-safe route handlers and conversions
* handlers are encoded with generic Haskell functions
* built on top of WAI and warp server libraries.
* provides Swagger to your server with one-line of code
* relies on standard classes to compose servers. The server is a monoid 
* we can build HTTP-clients from the server definition

Example of hello world server:

```haskell
{-# Language OverloadedStrings #-}

import Mig.Json.IO

-- | Starts server on port 8085.
main :: IO ()
main = runServer 8085 server

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

-- | The handler with a query parameter to ask for the user name
bye :: Query "user" Text -> Get (Resp Text)
bye (Query name) = pure $ ok ("Goodbye " <> name)
```

## How to start a new project

If you are a Haskell beginner and interested to try out building servers with `mig` 
the easiest way to start is to install [`stack`](https://docs.haskellstack.org/en/stable/).
See the main page of the `stack` docs in the link on how to do it. After the `stack` is installed we can generate a 
new `mig` project that contains hello world server with command:

```bash
> stack new my-project-name anton-k/hello-mig
```

It generates `my-project-name` directory that contains a code for our server.
Let's navigate to it, build server code and start the server:

```bash
> cd my-project-name
> make build
> make run
```

After that we can query the server on port 8085 either by curl or by swagger-ui.
The project contains a basic JSON API server with two routes. The code will be explained
in detail in the next chapter of this tutorial.

## How to use mig library in your project

We can install it from hackage. 
We need to use the library [mig-server](https://hackage.haskell.org/package/mig-server)

With cabal we can install it from Hackage:

```
cabal install mig-server --lib
```

With stack we can link to the repo in `extra-deps` (put it in your `stack.yaml`):

```yaml
extra-deps:
  - git: https://github.com/anton-k/mig
    commit: <some-commit-of-the-mig-libray>
    subdirs:
      - mig
      - mig-extra
      - mig-client
      - mig-wai
      - mig-swagger-ui
      - mig-server
```

## Structure of the library

There are several libraries:

- `mig` - core library which defines DSL to build servers with API-schemas and functions to render it to low-level representation. 
- `mig-extra` - extra add-ons to the core library
- `mig-server` - mig core with batteries and functions to run servers on top of warp.
- `mig-client` - HTTP-clients from the server code
- `mig-wai` - convert mig servers to WAI-applications
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

The mig uses the same ideas of type-safe handlers which are based on generic Haskell functions.
The main difference is that in servant the whole server is described as type. 
Which leads to type-safety and ability to derive API schema from the type.

But downside of it is fancy big types and very advanced concepts that user needs to know
in order to use the library. Also one drawback to me is when things go wrong and you get
several pages long error messages. If your server is really big it can be very hard to spot
the origin of the error as type mismatch is going to be with the whole type which describes 
the full server.

The mig borrows idea of type-safe functions to represent route handlers. 
But types represent only individual handlers. It does not describe the full server.
But we have type safety on the level of the single route. And error messages are going
to be localised and dedicated to a single route. 

Using type-level description of the routes provide the same benefits as in servant case:

* safe type check of the conversions of low level request and response elements
* usage of generic Haskell functions as handlers
* declarative design of the servers
* composition of servers from small sub-servers

In the mig API is a value that is derived from the server at run-time. 
It allows us to build clients and OpenApi swagger too.

To me servant is more demanding and complicated solution. I'd like to use 
something more simple.

### scotty

The scotty is also in domain of simple, easy to use solutions. 
So why did I wrote mig and haven't used the scotty instead?
Scotty features more imperative approach where you write handlers as 
expression for Scotty library monad. But it does not looks so well as in servant's case to me.
It is harder to assemble servers from parts. And I really like the idea of type-safe
conversions of various parts of request and response. 

So the scotty is simple enough but for me it lacks some servant features
such as composability of the servers (nice tree structure of the API)
and type-safe conversions of various parts of request and response.

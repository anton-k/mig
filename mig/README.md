## Core mig library

Core mig library provides tools to build HTTP servers and APIs.
With this library we can build lightweight and composable servers.
It is sort of servant for Simple/Boring haskell.

We build servers with type-safe DSL and we can convert a server
to low-level server representation and to OpenApi schema.

Servers can be rendered to low level representation of `ServerFun`:

```haskell
type ServerFun m = Request -> m (Maybe Response)
```

There is library `mig-wai` that can convert `ServerFun` to `Wai.Application`
and we can run it with `warp` (see library `mig-server` for that).

The HTTP Api can be converted to `OpenApi` value defined in `openapi3` library.

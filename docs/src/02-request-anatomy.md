# Anatomy of the request

For the next example we are going to try all sorts of inputs 
which are possible for the handler. 

## Useful presets for servers

Before we dive into various parts of the handler I'd like to introduce couple of
useful modules that make servers more specific. Often we don't need the most generic types.
If we know that all our servers will serve JSON and use only IO monad
we can use a special version of the `Mig` module:

```haskell
import Mig.Json.IO
```

It will provide several wrappers to simplify type signatures for handlers:

```haskell
type Get a = Send GET IO a
type Post a = Send POST IO a
```

also it provides more specific response type:

```haskell
newtype Resp a = Resp (Core.Resp Json a)
```

For the next example we are going to build JSON-application again.
So instea of more general `Mig` we will use `Mig.Json.IO`.

Also there are similiar modules for:

* `IO`-based servers
* `Html` servers with generic monad
* `Json` servers with generic monad
* `Json+IO` servers
* `Html+IO` servers

Servers for HTML take one step further and remove `Resp` from the equation:

```
type Get a = Send GET IO (Resp Html a)
type Post a = Send POST IO (Resp Html a)
```

There is one reason why we do not do that for JSON. But we will study it later.

## Http request

In previous example we could query by static path. Let's do something more funcy
and provide the input for the handler.

we have several types of inputs in HTTP:

* query parameters. We can see them in the path `"api/get/route?queryName=queryValue"`

* capture parameters. We can see them also in the path, but they are inlined 
   right into it: `api/get/route/someCaptureValueA/someCaptureValueB`

* header parameters. They are in HTTP-request headers. For example header that 
  reports media-type of the request body: "Content-Type: application/json"

* request body. It is avalue packed into HTTP-request. It can be JSON or text or raw string
   or XML. All sorts of things can be used as request bodies.

To use any of HTTP inputs in the handler we use special newtype wrappers 
as arguments to the handler functions. 

### Query parameter example

For example let's alter `hello` handler to greet not the `"World"` but someone
by the name:

```haskell
hello :: Query "who" Text -> Get (Resp Text)
hello (Query name) = Send $
  pure $ ok $ "Hello " <> name 
```

Note that we have imported `Mig.IO.Json` and our types are more
specific and have fewer arguments. All types are dedicated to `IO` and `Json`.
So we can write `Get (Resp Text)` instead of `Get IO (Resp Json Text)`.

Interesting part of the handler is that qrgument: `Query "who" Text`.
On the API level it creates expectation for a required query parameter in the path.
The `Query` is a simple newtype wrapper:

```haskell
newtype Query name value = Query value
```

The cool part of it is that code for the server does not change:

```haskell
server :: Server IO
server = "api/v1/hello" /. hello
```

There is no change because function `(/.)` is overloaded by second argument.
and it accepts all sorts of inputs. One of them states:

> if value `a` is convertible to server
> then `Query name value -> a` is also convertible to server

and by this magic as all haskell functions are curried we can use any number of
queries in the handler. For example if we want to greet two persons we can write:

```haskell
hello :: Query "personA" Text -> Query "personB" Text -> Get (Resp Text)
hello (Query nameA) (Query nameB) = Send $
  pure $ ok $ "Hello " <> nameA <> " and " <> nameB   
```

Also we can input any type if it has instance of the classes `FromHttpApiData` and `ToParamSchema`.
For example let's add two numbers:

```haskell
add :: Query "a" Int -> Query "b" Int -> Get (Resp Int)
add (Query a) (Query b) = Send $
  pure $ ok (a + b)
```

### The rest of the inputs

All other input parameters work in the same way as a `Query`. we have a newtype wrapper
for the value and type denotes all useful info for API description of the handler.

Let's for example query numbers for addition as capture parameters:

```haskell
add :: Capture "a" Int -> Capture "b" Int -> Get (Resp Int)
add (Query a) (Query b) = Send $
  pure $ ok (a + b)
```

It will expect the path to be `"api/v1/add/2/4"`. 
Other wrappers look very similiar:

* `Header name value` - for required headers
* `OptionalHeader name value` - for optional headers
* `Capture name value` - for path captures
* `Optional name value` - for optional queries
* `QueryFlag` - for booleab query that can be missing in the path (and then it is `false`)
* `Body media value` - for request body


### Using custom types as query parameters

The value of query parameter should have two instances of classes. We need:

* `FromHttpApiData` from the library [`http-api-data`](https://hackage.haskell.org/package/http-api-data-0.6/docs/Web-HttpApiData.html)
to convert to value from piece of the URL.

* `ToParamSchema` from the library [`openapi3`](https://hackage.haskell.org/package/openapi3) 
  to describe parameter type in the OpenApi schema.

Let's create a custom type and provide those instances:

```haskell
newtype AuthToken = AuthToken Text
  deriving newtype (FromHttpApiData, Eq, Ord, ToParamSchema)
```

We can derive them for `newtype` wrappers. Aftr that we can use `AuthToken` as value 
to get from query parameter. For more info on how to derive those instances see the docs for the libraries.
It's easy to do. We can derive `Generic` for the data type and derive `ToParamSchema` with it.

The same instances we need for all parameters-like inputs: queries, headers, captures.

### Nuances for Capture

The capture is interesting because it can be anywhere in the path.
for the example we havn't altered the server and our example:

```haskell
add :: Query "a" Int -> Query "b" Int -> Get (Resp Int)
add (Query a) (Query b) = Send $
  pure $ ok (a + b)

server = "api/v1/add" /. add
```

The server expects strings with template:

```
api/v1/add/{int}/{int}
```

So for missing captures it inserts them. It is the same as to write:

```haskell
server = "api/v1/add/*/*" /. add
```

We denote capture with `*`-wildcard. If we want the capture to be in another place 
in the path just put a star there:

```haskell
server = "api/v1/*/*/add-me" /. add
```

The server expects strings with template as path:

```
api/v1/{int}/{int}/add-me
```

### Json request body

I guess that JSON body as request is going to be the most popular case among all inputs.
So let's take a closer look at it as it often requires the custom type.

Let's add two numbers and provide input with request body:

```haskell
data AddInput = AddInput
  { a :: Int
  , b :: Int
  }
  deriving (Generic, FromJSON, ToSchema)

-- | Using JSON as body request
handleAddJson :: Body AddInput -> Post (Resp Int)
handleAddJson (Body (AddInput a b)) = Send $ 
  pure $ ok $ a + b
```

In the core mig library the type `Body` has two type arguments. But as we use Json specification
the first argument for `Mig.Json.IO` as for `Mig.Json` is always `Json`-tag.
So those modules provide special case alternative for type `Body`. But in the `mig`
library it uses the same idea as we saw in the query parameter. It is just a 
newtype wrapper for the value.

To be able to use it as input for the handler we have to provide instances for
several types:

* `FromJSON` from `aeson` library to parse value as JSON from byte string
* `ToSchema` from `openapi3` library to describe it in the API-schema

both of the types can be easily derived with `Generic` instance (from the module GHC.Generics).
First we derive instance of the `Generic` and then we can derive both `FromJSON` and `ToSchema`:

```haskell
data AddInput = AddInput
  { a :: Int
  , b :: Int
  }
  deriving (Generic, FromJSON, ToSchema)
```

also there are many libraries on hackage to 
create custom derivings for those classes: `deriving-aeson`, `aeson-deriving` and many others.

So to use JSON request body we can define our own type, derive proper classes and
we are done.

## Let's build a server

Let's recap on what we have learned and build server
with various request inputs:

```haskell
module Main (main) where

import Mig.Json.IO

main :: IO ()
main = runServer 8085 server

-- | Let's define a server
server :: Server IO
server = 
  "api"
  /. mconcat
    -- no args, constnat output
    [ "hello/world" /. helloWorld
    , -- required query param and custom header
      "succ" /. handleSucc
    , -- optional query param
      "succ-opt" /. handleSuccOpt
    , -- several query params
      "add" /. handleAdd
    , -- query flag
      "add-if" /. handleAddIf
    , -- capture
      "mul" /. handleMul
    , -- json body as input
      "add-json" /. handleAddJson
    ]

-- | Simple getter
helloWorld :: Get (Resp Text)
helloWorld = Send $ do
  pure $ ok "Hello world!"

newtype TraceId = TraceId Text
  deriving newtype (FromHttpApiData, ToHttpApiData, ToText, ToParamSchema)

{-| Using several inputs: header argument and required query
and using conditional output status
-}
handleSucc :: Header "Trace-Id" TraceId -> Query "value" Int -> Get (Resp Int)
handleSucc (Header _traceId) (Query n) = Send $ do
  pure $ ok (succ n)

-- | Using optional query parameters.
handleSuccOpt :: Optional "value" Int -> Get (Resp Int)
handleSuccOpt (Optional n) = Send $ do
  pure $ case n of
    Just val -> ok (succ val)
    Nothing -> ok 0 

{-| Using several query parameters
-}
handleAdd :: Query "a" Int -> Query "b" Int -> Get (Resp Int)
handleAdd (Query a) (Query b) = Send $ do
  pure $ ok $ a + b

-- | Using query flag if flag is false returns 0
handleAddIf :: Query "a" Int -> Query "b" Int -> QueryFlag "perform" -> Get (Resp Int)
handleAddIf (Query a) (Query b) (QueryFlag addFlag) = Send $ do
  pure $
    ok $
      if addFlag
        then (a + b)
        else 0

{-| Using capture as arguments. This route expects two arguments
captured in URL. For example:

> http://localhost:8085/hello/api/mul/3/100
-}
handleMul :: Capture "a" Int -> Capture "b" Int -> Get (Resp Int)
handleMul (Capture a) (Capture b) = Send $ do
  pure $ ok (a * b)

data AddInput = AddInput
  { a :: Int
  , b :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Using JSON as input
handleAddJson :: Body AddInput -> Post (Resp Int)
handleAddJson (Body (AddInput a b)) = Send $ do
  pure $ ok $ a + b
```

Curls to test the routes:

```
curl http://localhost:8085/api/hello/world

curl -X 'GET' \
  'http://localhost:8085/api/succ?value=2' \
  -H 'accept: application/json' \
  -H 'Trace-Id: xyz-trace'

curl -X 'GET' \
  'http://localhost:8085/api/add-if?a=2&b=4&perform=true' \
  -H 'accept: application/json'

curl -X 'GET' \
  'http://localhost:8085/api/mul/100/23' \
  -H 'accept: application/json'
```

## Adding some goodies to the servers

There are some useful addons that make development of the servers
much more pleasant. Let's discuss couple of them.

### Add swagger

Making `curl` request can quickly become hard to manage as
our servers become more complicated. There is OpenAPI standard 
that defines how to describe HTTP-server API. Also it provides
Swagger. It is a tool to make it easy to check how server behaves.
It pprovides an HTTP-client for the server which allows us to 
query server routes.

Let's add a swagger to our server. Just add this line:

```haskell
server :: IO
server = 
  withSwagger def $ 
    "api" /. 
      mcomcat [ {- the rest of the code -} ]
```

Let's add this line to our example and restart the server.
By default it creates a route for the server that serves Swagger UI client
at the path: [http://localhost:8085/swagger-ui/](http://localhost:8085/swagger-ui/).
It is easy to query the routes with swagger ui.

We can add swagger to any server with function:

```haskell
withSwagger :: SwaggerConfig m -> Server m -> Server m
```

We will study the `ServerConfig` in details in one of the next chapters
but for now the default value whcih is set with `def` from library `data-default`
is fine.

### Add simple logs to the server

We can look at the request and trsponse data with tracing functions
which come from library `mig-extra` from the module `Mig.Extra.Middleware.Trace`:

```haskell
data Verbosity = V0 | V1  | V2 | V3

-- log http requests and responses
logHttp :: Verbosity -> Middleware m

-- | log requests
logReq :: Verbosity -> Middleware m

-- | Log responses
logResp :: Verbosity -> Middleware m
```

The `Middleware m` is a function that can be applied to all routes of the server
and modify their behavior. To apply middleware to server we can use functions:

```haskell
applyMiddleware :: Middleware m -> Server m -> Server m

($:) :: Middleware m -> Server m -> Server m
```

We show simplified signatures here. The real ones are overloaded by the first argument.
but we will dicuss middlewares in depth in the separate chapter. For now it's
ok to assume that those functions are defined in that simplified way.

So let's look at the data that goes through our server:

```haskell
import Mig.Extra.Middleware.Trace qualified as Trace

...

server = 
  withSwagger def $ 
    withTrace $ {-# the rest of the server code #-}
  where
    withTrace = applyMiddleware (Trace.logHttp Trace.V2)
```

Let's restart the server and see what it logs:

```yaml
log:
  body: ''
  headers:
    accept: application/json
  method: GET
  path: api/add?a=12&b=45
  time: 2023-10-05T16:29:16.262934Z
  type: http-request

log:
  body: 57
  duration: 9.750000000000001e-4
  headers:
    content-type: application/json
  method: GET
  path: api/add?a=12&b=45
  status: 200
  time: 2023-10-05T16:29:16.263903Z
  type: http-response
```

This isan easy way to add addhock logs to the application.
Note that those logs are not aware of concurrency and will 
report intermingled messages on concurrent queries.

We can add real loggs with more generic versions of the functions
which accept callback and we can pass the logger function defined in terms
of one of the standard haskell logging libraries, say `katip` or `fast-logger`:

```haskell
import Data.Aeson as Json

logHttpBy :: (Json.Value -> m ()) -> Verbosity -> Middleware m
```

## Summary

We have learned how various parts of the requests can be queries
with newtype wrappers. There are only handful of them.
we can query

* `Query name value` - for required queries
* `Body media value` - for request body
* `Optional name value` - for optional queries
* `Header name value` - for required headers
* `OptionalHeader name value` - for optional headers
* `Capture name value` - for path captures
* `QueryFlag` - for booleab query that can be missing in the path (and then it is `false`)

We have learned to use specialized versions for servers which operate
only in terms of `IO` or `Json`. We can import the module `Mig.Json.IO`
and our signatures would bcome more simple and specific.

we have learned how by ony-liners we can add to the server some useful features:

* swagger: `(withSwagger def server)` 
    For calls to the server in the UI  

* trace logs: `(applyMiddleware (logHttp V2))` 
  To see the data that flows through the server

Both expressions transform servers and have signatures: 

```haskell
Server m -> Server m
```

# Anatomy of the request

For the next example we are going to try all sorts of inputs and outputs
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
queries in the hander. for example if we want greet two persons we can write:

```haskell
hello :: Query "personA" Text -> Query "personB" Text -> Get (Resp Text)
hello (Query nameA) (Query nameB) = Send $
  pure $ ok $ "Hello " <> nameA <> " and " <> nameB   
```

Also we can input anything of the instance of classes `FromHttpApiData` and `ToParamSchema`.
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

I guess that JSON body as request is going to be the most popular case among all inuts.
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

In the mig body has two type arguments. But as we use Json specification
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

## Http response

For short explanation sometimes things go bad and we would like to send errors.
For HTML we use the same type for errors and result values most of the time. It is an Html page.
But for JSON applications often errors would have different type than values.
And we have special type of response for that. That is why we would like to keep
the value type of the `Send` type general and not to restrict it as in HTML case.

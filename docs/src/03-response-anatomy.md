# Anatomy of the response

For the next example we are going to study which outputs can
handler produce. Let's study the HTTP-response.

## Http response

We already have seen the `Resp` data type in the first chapter:

```haskell
-- | Response with info on the media-type encoded as type.
data Resp media a = Resp
  { status :: Status
  -- ^ response status
  , headers :: ResponseHeaders
  -- ^ response headers
  , body :: Maybe a
  -- ^ response body. Nothing means "no content" in the body
  }
  deriving (Show, Functor)
```

It is the main type to return values and additional HTTP-information
from response.

An HTTP-response contains:

* integer status. It's 200 when everything is alright
* list of headers which provide useful info on response type
* the byte string body which contains result of handler operation. It can
   hold JSON, HTML, plain text, raw byte string and other types of outputs.

In the `Resp` type the `media` type argument specifies which
type the body has. By this type handler knows how to convert value
to low-level byte string representation.

### When things go bad


Sometimes things go bad and we would like to send errors and
state in the status the type of the error. To report errors
we have special type `RespOr`:

``` haskell
-- | Response that can contain an error. The error is represented 
-- with left case of an Either-type.

newtype RespOr ty err a = RespOr {unRespOr :: Either (Resp ty err) (Resp ty a)}
```

So this value has two possible responses which share the same media type.
We need two different responses to be able to report errors with different 
type than the type of the result.

### Response type class `IsResp`

To unify the output we have special type class called `IsResp` for
all types which can be converted to low-level HTTP-response type `Response`.

Let's study this type class.
It has two associated types for the type of the body (`RespBody`) and type of the error (`RespError`):

```haskell
class IsResp a where
  type RespBody a :: Type
  type RespError a :: Type
```

We can return successful result with method `ok`:

```haskell
  -- | Returns valid repsonse with 200 status
  ok :: RespBody a -> a
```

When things go bad we can report error with method `bad`:

```haskell
  -- | Returns an error with given status
  bad :: Status -> RespError a -> a
```

Sometimes at rare cases we do not what to return any content from response.
We can just report error status and leave the body empty:

```haskell
  -- | response with no content
  noContent :: Status -> a
```

We can add custom headers to the response by method `addHeaders`:

```haskell
  -- | Add some header to the response
  addHeaders :: ResponseHeaders -> a -> a
```

Note that header `Content-Type` is set automatically. Although sometimes
we would like set it explicitly. For that we have the method:

```haskell
  -- | Set the media type of the response
  setMedia :: MediaType -> a -> a
```

Also the core of the class is the method to convert value to low-level response:

```haskell
  -- | Converts value to low-level response
  toResponse :: a -> Response
```

Both `Resp` and `RespOr` are instances of `IsResp` class and 
we can `Send` as HTTP-response anything which has instance of `IsResp`.
For now there are only three types. The third one is the low-level `Response`.

## Examples

So we use `Resp` if we are sure that handler always produce a value
and we use `RespOr` if handler can produce and error.

### How to return error

We already have seen many usages of `Resp` type. Let's define something
that can produce an error. Let's define server that calculates
square root of the value. For negative numbers it is not defined in the 
realm of real numbers. So let's define the handler that use `RespOr` type:

```haskell
import Mig.Json.IO

server :: Server IO
server = 
  "square-root" /. squareRoot

squareRoot :: Body Float -> Post (RespOr Text Float)
squareRoot (Body arg) = pure $
  if arg >= 0 
    then ok (sqrt arg)
    else bad badRequest400 "Argument for square root should be non-negative"
```

So we return error message and bad request status 400 when 
negative argument is passed to the handler.

Also note this function looks like pure GET-type function but by the HTTP rules
we can not have body request in the GET-method. So we use POST instead.


Also we have special case function for bad requests called `badReq`. The
values for status come from the library http-types. See the module dedicated
to [HTTP-statuses](https://hackage.haskell.org/package/http-types-0.12.3/docs/Network-HTTP-Types-Status.html#t:Status).
It is reexported by the `mig` library.

### How to set headers

For example in the Header we expect trace id with which we can 
find the request and response in the logs. And we want to pass the
trace id from request to the response. Let's do it with `addHeaders`:


```haskell
passTrace :: Header "trace-id" Text -> Post (Resp ())
passTrace (Header traceId) =  
  pure $ addHeaders [("trace-id", toHeader traceId)] $ ok ()
```

The function `toHeader` is re-exported from the library [`http-api-data`](https://hackage.haskell.org/package/http-api-data-0.5/docs/Web-HttpApiData.html).
It converts various values to header byte string.

Also there is a function if we want to add only one header and not a list of them:

```haskell
setHeader :: (IsResp a, ToHttpApiData h) => HeaderName -> h -> a -> a
```

It has `toHeader` built into it.

Just like we set headers we also can set HTTP-status of the response.
We just apply it to Resp-like value. It works both for `Resp` and `RespOr`:

```haskell
setStatus :: IsResp a => Status -> a -> a
```

Although we rarely need this function as `ok` sets the right status 
for successful response and all functions that need the status take it as argument.

### How it works with server definition

How we can use both of the types as responses: `Resp` and `RespOr`.
Recall that `/.` function is overloaded by the second argument and
we have a rule for `ToServer` class that:

> if `a` has `IsResp` instance then `Send method m a` is convertible to server

As for both `Resp` and `RespOr` the instance for `IsResp` is defined we can use
both types as a result of the HTTP-handler.

## Summary

We have learned that there are only tow types to return from server handler:

* `Resp` for handlers that always produce a value
* `RespOr` for handlers that can fail

The need for the second type is to have different type of the error 
and different for the result. If both error and result have the same 
type then we can use `Resp`. This is common case for HTML servers when we
return HTML-page as result. In case of error we would like to show the page too
as in case of success. The difference would be in the HTTP-status of the response.

And this goes well with `IsResp` class as for `Resp media a` error type `RespError`
equals to `a` as the value for `RespBody` too.

Also we have learned various methods of the `IsResp` class and how they 
can be useful in server definitions.

With this chapter we have covered both requests and responses and which types the can 
have. See the source code [`RouteArgs`](https://github.com/anton-k/mig/blob/main/examples/mig-example-apps/RouteArgs/Main.hs)
for examples on the topic that we have just studied.



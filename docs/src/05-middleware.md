# Middlewares

A middleware is a transformation which is applied to all routes in the server.
It is a pair of functions which transform API-description and server function:

```haskell
data Middleware m = Middleware
  { info :: RouteInfo -> RouteInfo
  -- ^ update api schema
  , run :: MiddlewareFun m
  -- ^ update server function
  }

-- | Low-level middleware function.
type MiddlewareFun m = ServerFun m -> ServerFun m
```

To apply middleware to server we ca use function `applyMiddleware`:

```haskell
-- | Applies middleware to all routes of the server.
applyMiddleware :: forall f. (ToMiddleware f) => 
  f -> Server (MonadOf f) -> Server (MonadOf f)
```

There is also infix operatore for application `($:)`.

The class `ToMiddleware` contains all types that can be converted to middleware.
Here we use the same trick as with `ToServer` class to be able to read type-safe parts of the request
and update the API-schema. The type-level function `MonadOf` knows how to find underlying monad `m`
in various types.

We have recursive set of rules for types that can be converted to `Middleware`:

The identity rule:

> `MiddlewareFun` has instance of `ToMiddleware` with obvious identity instance

Recursive steps for inputs

> if `f` is `ToMiddleware` then `(Query name queryType -> f)` is `ToMiddleware` too

and so on for other types of request input (query params, headers, captures, request bodies).
See the full list of instances in the module `Mig.Core.Class.Middleware`.

## Examples

So the middleware allows us to apply some behavior to all routes in the server.
Let's discuss some examples

### Add logging

Let's add the logging to all methods which are called. We will log which route 
was called and we will include the time stamp and the full path in the log:

Let's imagine that we have a function

```haskell
logInfo :: Text -> IO ()
```

We can query the path with `PathInfo` newtype:

```haskell
newtype PathInfo = PathInfo [Text]
```

And we have a rule for  `ToMiddleware` class:

> if `f` is `ToMiddleware` then `(PathInfo -> ToMiddleware f)` is `ToMiddleware`

so we can create a middleware function:

```haskell
logRoutes :: Middleware IO
logRoutes = toMiddleware $ \(PathInfo pathItems) -> prependServerAction $ do
  now <- getCurrentTime 
  logInfo $ mconcat
    [ "Call route: ", Text.intercalata "/" pathItems 
    , " at ", Text.pack (show now)
    ]
```

We use function `prependServerAction` that creates a `Middleware`
from actino which is performed prior to call to server function:

```haskell
prependServerAction :: MonadIO m => m () -> Middleware m
```

also there are similar functions in the module: `appendServerAction` and `processResponse`.

### Allow only secure routes

Another great example of middleware at work is to block routes on some conditions.
For example if we want certain routes to be used only under secure SSL connection.
We have a standard function for that `whenSecure`. But let's dive into it's definition to
see how middlewares can be used:

```haskell
-- | Execute request only if it is secure (made with SSL connection)
whenSecure :: forall m. (MonadIO m) => Middleware m
whenSecure = toMiddleware $ \(IsSecure isSecure) -> 
  processResponse (if isSecure then id else const (pure Nothing))
```

Here we use standard middleware `processResponse` which allows
us to alter the result of the HTTP-response:

```haskell
processResponse :: MonadIO m => 
  (m (Maybe Response) -> m (Maybe Response)) -> Middleware m
```

Also we use query input `IsSecure` which is true if connection is made over SSL:

```haskell
newtype IsSecure = IsSecure Bool
```

So we pass through the response with identity if connection is secure
and we block the execution by returning `Nothing` if connection is secure.
The cool part of it is that due to Haskell's laziness there is no performance overhead and underlying
route is not going to be performed if connection is insecure.

### Authorization with middleware

Let's use this schema for authorization to site. 
There is a route that provides authorized users with session tokens.
User can pass credentials as request body over secure connection
and get session token in response which is valid for some time. 

With that token user can access the rest of the application.
User can pass token as special header. And we check in the application
that token is valid.

Imagine that we have a type for a session token:

```haskell
newtype AuthToken = AuthToken Text
    deriving newtype 
      (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show, ToParamSchema, ToSchema)
```

And we can get it from some route:

```haskell
getToken :: Body UserCreds -> Post (Resp AuthToken)
```

We would like to block invalid sessions for all routes of our site.
We can create it in similiar way as `whenSecure`:

```haskell
isValid :: AuthToken -> IO Bool
isValid = ...

headerAuth :: Header "auth" AuthToken -> Middleware IO
headerAuth (Header token) = processResponse $ \getResp -> do
  isOk <- isValid token
  if isOk
    then getResp
    else pure $ Just $ bad badRequest400 "Auth token is invalid"

whenAuth :: Server IO -> Server IO
whenAuth = applyMiddleware headerAuth
```

In this example we use `IsResp` instance for low-level http `Response`
to report authorization error. The header with name `"auth"` is required
for all routes which are part of the server to which we apply the middleware.

## Summary

In this chapter we have learned on middlewares. They provide a tool to apply
transformation to all routes in the server. Which can be useful for logging, authrization
and adding common behavior to all routes.

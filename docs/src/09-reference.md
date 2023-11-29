# Reference

Here is the list of main functions, types and classes

### How to build server

```haskell
-- server on path
(/.) :: ToServer a => Path -> a -> Server (MonadOf m)

-- alternative cases for server:
mconcat, (<>)

-- | take sub-server at path
atPath :: Path -> Server m -> Server m

toServer :: ToServer a => a -> Server (MonadOf m)
```

### How to run server

```haskell
runServer :: Int -> Server IO -> IO ()

runServer' :: ServerConfig -> Int -> Server IO -> IO ()
```

### Server presets

* `Mig` - generic types
* `Mig.IO` - `IO`-based servers with generic return types
* `Mig.Json` - JSON-based servers
* `Mig.Html` - HTML-based servers
* `Mig.Json.IO` - JSON and IO-based servers 
* `Mig.Html.IO` - HTML and IO-based servers 


### Media types

```haskell
* Json
* Text
* Html
* FormUrlEncoded
* AnyMedia
* OctetStream
```

### Request inputs

```haskell
-- required query parameter
newtype Body media value = Body value

-- required query parameter
newtype Query name value = Query value

-- optional query parameter
newtype Optional name value = Optional (Maybe value)

-- required header parameter
newtype Header name value = Header value

-- optional header parameter
newtype OptionalHeader name value = OptionalHeader (Maybe value)

-- capture in path parameter
newtype Capture name value = Capture value

-- boolean query flag parameter
newtype QueryFlag name = QueryFlag Bool

-- optional cookies (set in the header)
newtype Cookie value = Cookie (Maybe value)

-- Is connection made over SSL
newtype IsSecure = IsSecure Bool

-- full path with query parameters
newtype PathInfo = Path [Text]

-- | low-level request
newtype RawRequest = RawRequest Request
```

### Request outputs

```haskell
-- | generic route handler
newtype Send method m a = Send (m a)

-- Sepcific methos
type Get m a = Send GET m a
type Post m a = Send POST m a
...

-- Response where type of the value and error are the same
-- or only succesful result is expected
data Resp media a = Resp ...

-- Response where error and result have different types but media type is the same
data RespOr media err a = RespOr ...
```

The response type class:

```haskell
class IsResp a where
  -- | the type of response body value
  type RespBody a :: Type

  -- | the type of an error
  type RespError a :: Type

  -- | Returns valid repsonse with 200 status
  ok :: RespBody a -> a

  -- | Returns an error with given status
  bad :: Status -> RespError a -> a

  -- | response with no content
  noContent :: Status -> a

  -- | Add some header to the response
  addHeaders :: ResponseHeaders -> a -> a

  -- | Sets repsonse status
  setStatus :: Status -> a -> a

  -- | Set the media type of the response
  setMedia :: MediaType -> a -> a

  -- | Reads the media type by response type
  getMedia :: MediaType

  -- | Converts value to low-level response
  toResponse :: a -> Response

setHeader :: (IsResp a, ToHttpApiData h) => HeaderName -> h -> a -> a

-- | Set cookie as http header from form url encoded value
setCookie :: (ToForm cookie, IsResp resp) => SetCookie cookie -> resp -> resp

data SetCookie a = SetCookie
  { cookie :: a
  , expires :: Maybe UTCTime
  , domain :: Maybe Text
  , path :: Maybe Text
  , secure :: Bool
  , httpOnly :: Bool
  }

-- | Default cookie which sets only the cookie itself.
defCookie :: a -> SetCookie a

-- | Bad request. The @bad@ response with 400 status.
badReq :: (IsResp a) => RespError a -> a

-- | Internal server error. The @bad@ response with 500 status.
internalServerError :: (IsResp a) => RespError a -> a

-- | Not implemented route. The @bad@ response with 501 status.
notImplemented :: (IsResp a) => RespError a -> a

-- | Redirect to url. It is @bad@ response with 302 status 
-- and set header of "Location" to a given URL.
redirect :: (IsResp a) => Text -> a
```

### Plugins

```haskell
applyPlugin, ($:) :: ToPlugin a => 
  a -> Server (MonadOf a) -> Server (MonadOf a)

-- composition of plugins:
Monoid(..): mconcat, (<>), mempty
```

### specific servers

```haskell
-- | add wagger to server
withSwagger :: SwaggerConfig m -> Server m -> Server m

-- | add link from one route to another
addPathLink :: Path -> Path -> Server m

-- static files
staticFiles :: [(FilePath, ByteString)] -> Server m
```

### specific plugins


```haskell
-- prepend or append some acction to all routes
prependServerAction, appendServerAction :: MonadIO m => m () -> Plugin m

-- change the response
processResponse :: (m (Maybe Response) -> m (Maybe Response)) -> Plugin m

-- only secure routes are allowed
whenSecure :: forall m. (MonadIO m) => Plugin m

-- logging with putStrLn for debug traces
logHttp :: Verbosity -> Plugin m

-- logging with custom logger
logHttpBy :: (Json.Value -> m ()) -> Verbosity -> Plugin m

-- | simple authorization
withHeaderAuth :: WithHeaderAuth -> Plugin m
```

### How to use Reader

```haskell
-- Derive instance of HasServer class for your Reader-IO based application:
newtype App a = App (ReaderT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, HasServer)

renderServer :: Server App -> Env -> IO (Server IO)
```

### OpenApi and Swagger

```haskell
-- | Get OpenApi
toOpenApi :: Server m -> OpenApi

-- add swagger to server
withSwagger :: SwaggerConfig m -> Server m -> Server m

-- create swagger server
swagger :: SwaggerConfig m -> m OpenApi -> Server m

-- | Print OpenApi schema
printOpenApi :: Server m -> IO ()

-- | Writes openapi schema to file
writeOpenApi :: FilePath -> Server m -> IO ()
```
The Swagger config:

```haskell
-- | Swagger config
data SwaggerConfig m = SwaggerConfig
  { staticDir :: Path
  -- ^ path to server swagger (default is "/swagger-ui")
  , swaggerFile :: Path
  -- ^ swagger file name (default is "swaggger.json")
  , mapSchema :: OpenApi -> m OpenApi
  -- ^ apply transformation to OpenApi schema on serving OpenApi schema.
  -- it is useful to add additional info or set current date in the examples
  -- or apply any real-time transformation.
  }

instance (Applicative m) => Default (SwaggerConfig m) where
  def =
    SwaggerConfig
      { staticDir = "swagger-ui"
      , swaggerFile = "swagger.json"
      , mapSchema = pure
      }
```

Set swagger title and description:

```haskell
-- | Default info that is often added to OpenApi schema
data DefaultInfo = DefaultInfo
  { title :: Text
  , description :: Text
  , version :: Text
  }

-- adds default info, use it in the mapSwagger field of SwaggerConfig record
addDefaultInfo :: DefaultInfo -> OpenApi -> OpenApi
```

Describe routes with swagger:

```haskell
-- | Sets description of the route
setDescription :: Text -> Server m -> Server m

-- | Sets summary of the route
setSummary :: Text -> Server m -> Server m

-- | Adds OpenApi tag to the route
addTag :: Text -> Server m -> Server m

{-| Appends descriptiton for the inputs. It passes pairs for @(input-name, input-description)@.
special name request-body is dedicated to request body input
nd raw-input is dedicated to raw input
-}
describeInputs :: [(Text, Text)] -> Server m -> Server m
```


## Clients

The class to convert server definitions to clients:

```haskell
class (MapRequest a) => ToClient a where
  -- | converts to client function
  toClient :: Server m -> a

  -- | how many routes client has
  clientArity :: Int
```

An example:

```haskell
helloWorld :: Get Client (Resp Text)
handleSucc :: Header "Trace-Id" TraceId -> Query "value" Int -> Get Client (Resp Int)
handleSuccOpt :: Optional "value" Int -> Get Client (RespOr Text Int)

helloWorld
  :| handleSucc
  :| handleSuccOpt = toClient server
```

We use synonym for pair `:|` to avoid redundant parens.

The Client-monad:

```haskell
newtype Client a = ...

runClient :: ClientConfig -> Client a -> IO (RespOr AnyMedia BL.ByteString a)

-- | Config to run the clients
data ClientConfig = ClientConfig
  { port :: Int
  -- ^ port to connect to
  , manager :: Http.Manager
  -- ^ HTTP-manager
  }
```

The class to strip away request input `newtype` wrappers:

```haskell
class FromClient a where
  type ClientResult a :: Type
  fromClient :: a -> ClientResult a
```
It turns types like:

```haskell
Query "a" Int -> Capture "b" Text -> Get Client (Resp Json Text)
```

To types:

```haskell
Int -> Text -> Client' (RespOr Json BL.ByteString Text)
```


Where `Client'` is a monad which encapsulates the `ClientConfig` as reader:

```haskell
newtype Client' a = Client' (ReaderT ClientConfig IO a)
```

Also we can use the function:

```haskell
getRespOrValue :: RespOr media BL.ByteString a -> Either BL.ByteString a
```

To unwrap `Resp` from response.

## Other utilities

### Type-safe URLs

The type-level function `UrlOf` creates a type-safe `Url` for a given route handler type.
With class `ToUrl` we can generate the URLs for a collection of handlers.

```haskell
class ToUrl a where
  toUrl :: Server m -> a
  mapUrl :: (Url -> Url) -> a -> a
  urlArity :: Int
```

An example of usage.
URL's should be listed in the same order as they appear in the server

```haskell
urls :: Urls
urls = Urls{..}
   where
     greeting
       :| blogPost
       :| listPosts
         toUrl (server undefined)
```

We can render `Url` to `String`-like type with function:

```haskell
renderUrl :: IsString a => Url -> a
```

### deriving helpers

Sometimes we need to derive too many types at once
to use the type in the library. There are helpers to reduce deriving boiler-plate:


```haskell
deriveParam ''FooType          -- derives parameter instances for a FooType
deriveNewtypeParam ''FooType   -- derives parameter instances for a newtype FooType

deriveBody ''FooType          -- derives request body instances for a FooType
deriveNewtypeBody ''FooType   -- derives request body instances for a newtype FooType

deriveHttp ''FooType          -- derives both parameter and request body instances for a FooType
deriveNewtypeHttp ''FooType   -- derives both parameterrequest body instances for a newtype FooType

mapDerive fun [''Foo, ''Bar]  -- maps deriving over several types
```
We need to activate `TemplateHaskell`, `StandaloneDeriving`, `DerivingStrategies`, `DeriveGeneric` extensions to use it.

Also note that for this to work all types should be in scope. 
So it better to define drivings at the bottom of the module which is dedicated to types.

Also type should not have generic arguments for deriving to work.
If it does we have to declare the types manually. For example:

```haskell
data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

deriving instance (ToSchema a) => ToSchema (Timed a)
```

The data type `Timed` has an argument and we have to define the instance 
explicitly.

### HTML Links

For usage with template engines that expect JSON as argument for template
there is the type `Link` in the module `Mig.Extra.Html` (also re-exported by all `Html`-related modules):

```haskell
data Link = Link
  { href :: Url
  , name :: Text
  }
  deriving (Generic, ToJSON)
```

Also it has `ToMarkup` instance and rendered as `a`-element link.

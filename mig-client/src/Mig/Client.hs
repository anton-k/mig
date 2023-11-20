{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions to create http-clients from the same code as server or API schema
module Mig.Client (
  ToClient (..),
  Client (..),
  ClientConfig (..),
  runClient,
  (:|) (..),
  FromClient (..),
  getRespOrValue,
  Client' (..),
  runClient',
  MonadIO (..),
  ClientOr,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.String
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.TypeLits
import Network.HTTP.Client qualified as Http
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Web.HttpApiData

import Mig.Core

instance (ToClient a, ToClient b) => ToClient (a :| b) where
  toClient api = a :| b
    where
      (a, b) = toClient api

  clientArity = clientArity @(a, b)

class MapRequest a where
  mapRequest :: (Http.Request -> Http.Request) -> (a -> a)
  mapCapture :: (CaptureMap -> CaptureMap) -> (a -> a)

instance MapRequest (Client a) where
  mapRequest f (Client a) = Client (\conf capt -> a conf capt . f)
  mapCapture f (Client a) = Client (\conf capt req -> a conf (f capt) req)

instance MapRequest (Send method Client a) where
  mapRequest f (Send client) = Send (mapRequest f client)
  mapCapture f (Send client) = Send (mapCapture f client)

instance (MapRequest b) => MapRequest (a -> b) where
  mapRequest f a = mapRequest f . a
  mapCapture f a = mapCapture f . a

instance (MapRequest a, MapRequest b) => MapRequest (a, b) where
  mapRequest f (a, b) = (mapRequest f a, mapRequest f b)
  mapCapture f (a, b) = (mapCapture f a, mapCapture f b)

instance (MapRequest a, MapRequest b) => MapRequest (a :| b) where
  mapRequest f (a :| b) = (mapRequest f a :| mapRequest f b)
  mapCapture f (a :| b) = (mapCapture f a :| mapCapture f b)

instance (MapRequest a, MapRequest b, MapRequest c) => MapRequest (a, b, c) where
  mapRequest f (a, b, c) = (mapRequest f a, mapRequest f b, mapRequest f c)
  mapCapture f (a, b, c) = (mapCapture f a, mapCapture f b, mapCapture f c)

instance (MapRequest a, MapRequest b, MapRequest c, MapRequest d) => MapRequest (a, b, c, d) where
  mapRequest f (a, b, c, d) = (mapRequest f a, mapRequest f b, mapRequest f c, mapRequest f d)
  mapCapture f (a, b, c, d) = (mapCapture f a, mapCapture f b, mapCapture f c, mapCapture f d)

{-| Creates http-client from server definition.

The result adapts to decalred types. It creates so many client functions as
the arity of the tuple in the result. The server can have more handlers than in the result
Routes from result definition and server definition are matched in the same order as they are declared.

The type of the client is derived from the type signature of the result. The information
on paths for handlers is derived from server definition.

To use the same code for both client and server it is convenient to declare
signatures for handlers as type synonyms parameterized by server monad.
And in the server implementation monad is going to be something IO-based but
in client handlers it will be `Client`-monad.

For example for a server:

> type Hello m = Capture "name" Text -> Get m (Resp Text)
> type Add m = Query "a" Int -> Query "b" Int -> Get m (Resp Int)
>
> server :: Server IO
> server = "api" /.
>   mconcat
>    [ "hello" /. helloHandler
>    , "add" /. addHandler
>    ]
>
> helloHandler :: Hello IO
> helloHandler (Capture name) = Send $ pure $ ok $ "Hello " <> name
>
> addHandler :: Add IO
> addHandler (Query a) (Query b) = Send $ pure $ ok (a + b)

We can define the client and reuse type signatures that we have defined in the server code:

> helloClient :: Hello Client
> addClient :: Add Client
>
> helloClient :| addClient = toClient server

If there is no definition for server. For example if we write implementation for
some external server or API provided by third party we can use recursive definition in the server.
For example if there is no haskell implementation for the server in the previous example. But we
know the API of the application we can define client with recursive definition:

> type Hello m = Capture "name" Text -> Get m (Resp Text)
> type Add m = Query "a" Int -> Query "b" Int -> Get m (Resp Int)
>
> helloClient :: Hello Client
> addClient :: Add Client
>
> helloClient :| addClient = toClient server
>
> server :: Server Client
> server = "api" /.
>   mconcat
>    [ "hello" /. helloClient
>    , "add" /. addClient
>    ]

The code does not get stuck into recursion loop because implementation of the route handlers
is not needed to create client functions. The function @toClient@ takes into account
only type-signatures of the handlers and paths.
-}
class (MapRequest a) => ToClient a where
  -- | converts to client function
  toClient :: Server m -> a

  -- | how many routes client has
  clientArity :: Int

-- | Config to run the clients
data ClientConfig = ClientConfig
  { port :: Int
  -- ^ port to connect to
  , manager :: Http.Manager
  -- ^ HTTP-manager
  }

-- | The client monad. All errors are unified to Lazy.ByteString.
newtype Client a = Client (ClientConfig -> CaptureMap -> Http.Request -> IO (RespOr AnyMedia BL.ByteString a))
  deriving (Functor)

instance Applicative Client where
  pure a = Client $ const $ const $ const $ pure $ pureResp a
  (<*>) = ap

pureResp :: a -> RespOr AnyMedia BL.ByteString a
pureResp a = RespOr $ Right $ Resp ok200 [] (Just a)

instance Monad Client where
  (Client ma) >>= mf = Client $ \config captureValues req -> do
    RespOr eResp <- ma config captureValues req
    case eResp of
      Right resp -> case resp.body of
        Just body -> case mf body of
          Client run -> run config captureValues req
        Nothing -> pure (RespOr $ Right $ Resp resp.status resp.headers Nothing)
      Left err -> pure (RespOr $ Left err)

instance MonadIO Client where
  liftIO act = Client $ \_ _ _ -> pureResp <$> act

-- | Runs client. It calls client handler and fetches the result.
runClient :: ClientConfig -> Client a -> IO (RespOr AnyMedia BL.ByteString a)
runClient config (Client act) = act config mempty Http.defaultRequest

instance (IsMethod method, FromReqBody (RespMedia a) (RespBody a), IsResp a) => ToClient (Send method Client a) where
  toClient api =
    mapRequest (setRequestMethod (toMethod @method)) $
      Send $
        fmap ok $
          httpSend @(RespMedia a) @(RespBody a) (getHeadPath $ Server $ fillCaptures $ unServer api)

  clientArity = 1

instance (ToClient a, ToClient b) => ToClient (a, b) where
  toClient (Server api) = (toClient (Server apiA), toClient (Server apiB))
    where
      (apiA, apiB) = bimap fromFlatApi fromFlatApi $ Prelude.splitAt (clientArity @a) (flatApi api)

  clientArity = clientArity @a + clientArity @b

instance (ToClient a, ToClient b, ToClient c) => ToClient (a, b, c) where
  toClient api = (a, b, c)
    where
      (a, (b, c)) = toClient api

  clientArity = clientArity @(a, (b, c))

instance (ToClient a, ToClient b, ToClient c, ToClient d) => ToClient (a, b, c, d) where
  toClient api = (a, b, c, d)
    where
      (a, (b, c, d)) = toClient api

  clientArity = clientArity @(a, (b, c, d))

getHeadPath :: Server m -> Path
getHeadPath (Server api) = case flatApi api of
  (pathHead, _) : _ -> pathHead
  _ -> error "Not enought methods. API is empty"

setRequestMethod :: Method -> Http.Request -> Http.Request
setRequestMethod m req = req{Http.method = m}

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Header sym a -> b) where
  toClient api = \header -> mapRequest (addHeader header) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (OptionalHeader sym a -> b) where
  toClient api = \header -> mapRequest (addOptionalHeader header) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Query sym a -> b) where
  toClient api = \query -> mapRequest (addQuery query) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToClient b) => ToClient (QueryFlag sym -> b) where
  toClient api = \(QueryFlag flag) -> mapRequest (addQuery @sym @Bool (Query flag)) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Optional sym a -> b) where
  toClient api = \query -> mapRequest (addOptional query) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Capture sym a -> b) where
  toClient api = \capture -> mapCapture (addCapture capture) $ toClient @b api
  clientArity = clientArity @b

instance (ToRespBody media a, ToClient b) => ToClient (Body media a -> b) where
  toClient api = \(Body body) -> mapRequest (addBody @media body) $ toClient @b api
  clientArity = clientArity @b

instance (ToClient b) => ToClient (PathInfo -> b) where
  toClient api = \(PathInfo _path) -> toClient @b api
  clientArity = clientArity @b

instance (ToClient b) => ToClient (IsSecure -> b) where
  toClient api = \(IsSecure _val) -> toClient @b api
  clientArity = clientArity @b

instance (ToClient b) => ToClient (RawRequest -> b) where
  toClient api = \(RawRequest _val) -> toClient @b api
  clientArity = clientArity @b

instance (ToClient b) => ToClient (RawResponse -> b) where
  toClient api = \(RawResponse _val) -> toClient @b api
  clientArity = clientArity @b

addQuery :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Query sym a -> Http.Request -> Http.Request
addQuery (Query a) req = req{Http.queryString = str}
  where
    str =
      if B.null (Http.queryString req)
        then param
        else Http.queryString req <> "&" <> param

    param = fromString (symbolVal (Proxy @sym)) <> "=" <> Text.encodeUtf8 (toUrlPiece a)

addOptional :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Optional sym a -> Http.Request -> Http.Request
addOptional (Optional mVal) = case mVal of
  Nothing -> id
  Just val -> addQuery @sym (Query val)

addHeader :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Header sym a -> Http.Request -> Http.Request
addHeader (Header a) = addRequestHeader (fromString $ symbolVal (Proxy @sym), toHeader a)

addOptionalHeader :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => OptionalHeader sym a -> Http.Request -> Http.Request
addOptionalHeader (OptionalHeader ma) = case ma of
  Nothing -> id
  Just a -> addHeader @sym (Header a)

addCapture :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Capture sym a -> CaptureMap -> CaptureMap
addCapture (Capture a) =
  Map.insert (fromString (symbolVal (Proxy @sym))) (toUrlPiece a)

addBody :: forall media a. (ToRespBody media a) => a -> Http.Request -> Http.Request
addBody body req =
  addRequestHeader ("Content-Type", renderHeader (toMediaType @media)) $
    req{Http.requestBody = Http.RequestBodyLBS (toRespBody @media body)}

addRequestHeader :: (HeaderName, ByteString) -> Http.Request -> Http.Request
addRequestHeader header req = req{Http.requestHeaders = header : Http.requestHeaders req}

pathToString :: CaptureMap -> Path -> ByteString
pathToString captureValues (Path path) = case path of
  [] -> mempty
  item : rest -> flip rec (Path rest) $ case item of
    StaticPath p -> p
    CapturePath p ->
      case Map.lookup p captureValues of
        Just val -> val
        Nothing -> error $ "No value for capture: " <> Text.unpack p
  where
    rec a rest = Text.encodeUtf8 a <> "/" <> pathToString captureValues rest

httpSend :: forall media a. (FromReqBody media a) => Path -> Client a
httpSend path =
  mapRequest (addRequestHeader ("Accept", renderHeader $ toMediaType @media)) $
    Client $ \config captureValues req ->
      toSend <$> Http.httpLbs (setRoute captureValues path $ setPort config.port req) config.manager
  where
    toSend :: Http.Response BL.ByteString -> RespOr AnyMedia BL.ByteString a
    toSend resp =
      RespOr $ case fromReqBody @media $ Http.responseBody resp of
        Right body -> Right $ toResp body
        Left _ -> Left $ toResp $ Http.responseBody resp
      where
        toResp :: val -> Resp AnyMedia val
        toResp = Resp (Http.responseStatus resp) (Http.responseHeaders resp) . Just

setPort :: Int -> Http.Request -> Http.Request
setPort port req = req{Http.port = port}

setRoute :: CaptureMap -> Path -> Http.Request -> Http.Request
setRoute captureValues path req = req{Http.path = pathToString captureValues path}

----------------------------------------------------------
-- from response

-- | Helper type-synonym for convenience
type ClientOr a = Client' (Either BL.ByteString a)

{-| ClientConfig in @ReaderT IO@ monad. It encapsulates typical execution
of client functions
-}
newtype Client' a = Client' (ReaderT ClientConfig IO a)
  deriving (Functor, Applicative, Monad, MonadReader ClientConfig, MonadIO)

-- | Runs the client call
runClient' :: ClientConfig -> Client' a -> IO a
runClient' config (Client' act) = runReaderT act config

{-| Class to strip away all newtype wrappers that serve for API-definition.
For example it converts the types signature for client function:

> Capture "foo" Text -> Header "bar" Int -> Get Client (Resp a)

to the version without HTTP-newtype wrappers:

> Text -> Int -> Client' (Resp a)

The instances are defined for all HTTP-newtype wrappers.
Also we can use function @getRespOrValue@ if we do not need
the http information of response.
-}
class FromClient a where
  type ClientResult a :: Type
  fromClient :: a -> ClientResult a

instance (ToRespBody (RespMedia a) (RespError a), IsResp a) => FromClient (Send method Client a) where
  type ClientResult (Send method Client a) = Client' (RespOr (RespMedia a) BL.ByteString (RespBody a))
  fromClient f = Client' $ ReaderT $ flip fromSendClient f

instance (FromClient b) => FromClient (Body media a -> b) where
  type ClientResult (Body media a -> b) = a -> ClientResult b
  fromClient f arg = fromClient (f (Body arg))

instance (FromClient b) => FromClient (Capture sym a -> b) where
  type ClientResult (Capture sym a -> b) = a -> ClientResult b
  fromClient f arg = fromClient (f (Capture arg))

instance (FromClient b) => FromClient (Query sym a -> b) where
  type ClientResult (Query sym a -> b) = a -> ClientResult b
  fromClient f arg = fromClient (f (Query arg))

instance (FromClient b) => FromClient (QueryFlag a -> b) where
  type ClientResult (QueryFlag a -> b) = Bool -> ClientResult b
  fromClient f arg = fromClient (f (QueryFlag arg))

instance (FromClient b) => FromClient (Optional sym a -> b) where
  type ClientResult (Optional sym a -> b) = Maybe a -> ClientResult b
  fromClient f arg = fromClient (f (Optional arg))

instance (FromClient b) => FromClient (Header sym a -> b) where
  type ClientResult (Header sym a -> b) = a -> ClientResult b
  fromClient f arg = fromClient (f (Header arg))

instance (FromClient b) => FromClient (OptionalHeader sym a -> b) where
  type ClientResult (OptionalHeader sym a -> b) = Maybe a -> ClientResult b
  fromClient f arg = fromClient (f (OptionalHeader arg))

instance (FromClient b) => FromClient (PathInfo -> b) where
  type ClientResult (PathInfo -> b) = ClientResult b
  fromClient f = fromClient @b (f $ PathInfo [])

instance (FromClient b) => FromClient (IsSecure -> b) where
  type ClientResult (IsSecure -> b) = ClientResult b
  fromClient f = fromClient @b (f (IsSecure True))

instance (FromClient b) => FromClient (RawRequest -> b) where
  type ClientResult (RawRequest -> b) = ClientResult b
  fromClient f = fromClient @b (f $ error "no request")

instance (FromClient b) => FromClient (RawResponse -> b) where
  type ClientResult (RawResponse -> b) = ClientResult b
  fromClient f = fromClient @b (f $ error "no response")

fromSendClient ::
  forall a method.
  (ToRespBody (RespMedia a) (RespError a), IsResp a) =>
  ClientConfig ->
  Send method Client a ->
  IO (RespOr (RespMedia a) BL.ByteString (RespBody a))
fromSendClient config (Send client) =
  joinRespOr @a <$> runClient config client

joinRespOr :: forall a. (ToRespBody (RespMedia a) (RespError a), IsResp a) => RespOr AnyMedia BL.ByteString a -> RespOr (RespMedia a) BL.ByteString (RespBody a)
joinRespOr (RespOr eResp) = RespOr $ case eResp of
  Right resp -> case resp.body of
    Just result ->
      if getStatus result == ok200
        then Right $ Resp (getStatus result) (getHeaders result) (getRespBody result)
        else Left $ Resp (getStatus result) (getHeaders result) (toRespBody @(RespMedia a) <$> getRespError result)
    Nothing -> Right $ Resp resp.status resp.headers Nothing
  Left resp -> Left $ Resp resp.status resp.headers resp.body

{-| If we need only value from the server and not HTTP-info (status, or headers)
we can omit that data with this function
-}
getRespOrValue :: RespOr media BL.ByteString a -> Either BL.ByteString a
getRespOrValue (RespOr eResp) = case eResp of
  Right resp -> maybe noContentValue Right resp.body
  Left resp -> maybe noContentValue Left resp.body
  where
    noContentValue = Left "No content in the response"

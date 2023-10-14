{-# Language UndecidableInstances #-}
-- | Main module to write servers
--
-- Server is a function from response (Resp) to request (Req). Request is wrapped into monad.
-- Library supports IO-monad and ReaderT over IO like monads.
--
-- We can build servers from parts with flexible combinators.
-- Let's build hello-world server:
--
-- > main :: IO ()
-- > main = runServer 8080 server
-- >
-- > server :: Server IO
-- > server =
-- >   "api" /. "v1" /. "hello" /. Get @Text handleHello
-- >
-- > handleHello :: IO Text
-- > handleHello = pure "Hello World"
--
-- We can iuse monoids to combine servers and newtype-wrappers to read various inputs.
-- See readme of the repo for tutorial and docs.
module Mig
  ( -- * types
    Server (..)
  -- * DSL
  , Json
  -- ** methods
  , Get (..)
  , Post (..)
  , Put (..)
  , Delete (..)
  , Patch (..)
  , Options (..)
  -- ** path and query
  -- | Build API for routes with queries and captures.
  -- Use monoid to combine several routes together.
  , (/.)
  , Capture (..)
  , Query (..)
  , Optional (..)
  , Body (..)
  , RawBody (..)
  , Header (..)
  , FormBody (..)
  , PathInfo (..)

  -- ** response
  -- | How to modify response and attach specific info to it
  , AddHeaders (..)
  , SetStatus (..)
  , setStatus
  , addHeaders

  -- ** Errors
  -- | How to report errors
  , Error (..)
  , handleError

  -- * Run
  -- | Run server application
  , runServer
  , ServerConfig (..)
  , toApplication

  -- ** Render
  -- | Render Reader-IO monad servers to IO servers.
  , HasServer (..)
  , fromReader

  -- * Convertes
  , ToTextResp (..)
  , ToJsonResp (..)
  , ToHtmlResp (..)
  , FromText (..)
  , ToText (..)

  -- * utils
  , badRequest
  , ToServer (..)
  , withServerAction

  , module X
  ) where

import Mig.Internal.Types
import Mig.Internal.Types qualified as Resp (Resp (..))

import Web.HttpApiData as X
import Web.FormUrlEncoded as X
import Data.Bifunctor
import Data.Kind
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Text.Blaze.Html (Html)
import Text.Blaze.Html (ToMarkup)
import Text.Read (readMaybe)
import Control.Monad.Reader
import Control.Monad.Except (ExceptT, runExceptT)
import GHC.TypeLits
import Data.Proxy
import Data.Map.Strict qualified as Map
import Network.HTTP.Types.Status as X
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.Wai.Handler.Warp qualified as Warp
import Control.Exception (throw)
import Data.Typeable
import Control.Monad ((<=<))

-- | Path constructor (right associative). Example:
--
-- > server :: Server IO
-- > server =
-- >   "api" /. "v1" /.
-- >      mconcat
-- >        [ "foo" /. Get  @Json handleFoo
-- >        , "bar" /. Post @Json handleBar
-- >        ]
-- >
-- > handleFoo, handleBar :: IO Text
(/.) :: ToServer a => Text -> a -> Server (ServerMonad a)
(/.) path act = toWithPath path (toServer act)
infixr 4 /.

-- | Map internal monad of the server
hoistServer :: (forall a . m a -> n a) -> Server m -> Server n
hoistServer f (Server act) = Server (f . act)

-- | Aything convertible from text
class FromText a where
  fromText :: Text -> Maybe a

instance FromText ByteString where
  fromText = Just . Text.encodeUtf8

instance FromText String where
  fromText = Just . Text.unpack

instance FromText Text where
  fromText = Just

instance FromText TL.Text where
  fromText = Just . TL.fromStrict

instance FromText Word where
  fromText = readMaybe . Text.unpack

instance FromText Int where
  fromText = readMaybe . Text.unpack

instance FromText Integer where
  fromText = readMaybe . Text.unpack

instance FromText Bool where
  fromText = readMaybe . Text.unpack

instance FromText Float where
  fromText = readMaybe . Text.unpack

newtype QueryName a = QueryName Text
  deriving (IsString, Eq, Ord, Show)

toWithQuery :: ByteString -> (Maybe ByteString -> Server m) -> Server m
toWithQuery name act = Server $ \req ->
  unServer (act (Map.lookup name req.query)) req

withQuery' :: FromHttpApiData a => QueryName a -> (Maybe a -> Server m) -> Server m
withQuery' (QueryName name) act = toWithQuery (Text.encodeUtf8 name) $ \mVal ->
  let
    -- TODO: do not ignore parse failure
    mArg = either (const Nothing) Just . (parseQueryParam <=< first (Text.pack . show) . Text.decodeUtf8') =<< mVal
  in
    act mArg

withQuery :: (Applicative m, FromHttpApiData a) => QueryName a -> (a -> Server m) -> Server m
withQuery (QueryName name) act = toWithQuery (Text.encodeUtf8 name) $ \mVal ->
  let
    -- TODO: do not ignore parse failure
    mArg = either (const Nothing) Just . (parseQueryParam <=< first (Text.pack . show) . Text.decodeUtf8') =<< mVal
  in
    case mArg of
      Just arg -> act arg
      Nothing -> toConst (pure $ badRequest $ "Failed to parse arg: " <> name)

-- | Class contains types which can be converted to IO-based server to run as with WAI-interface.
--
-- We can run plain IO-servers and ReaderT over IO based servers. Readers can be wrapped in newtypes.
-- In that case we can derive automatically @HasServer@ instance.
class Monad m => HasServer m where
  type ServerResult m :: Type
  renderServer :: Server m -> ServerResult m

instance HasServer IO where
  type ServerResult IO = Server IO
  renderServer = id

instance HasServer (ReaderT env IO) where
  type ServerResult (ReaderT env IO) = env -> IO (Server IO)
  renderServer server initEnv = fromReader initEnv server

-- | Render reader server to IO-based server
fromReader :: env -> Server (ReaderT env IO) -> IO (Server IO)
fromReader env server =
  flip runReaderT env $ ReaderT $ \e -> pure $ hoistServer (flip runReaderT e) server

instance (Show a, Typeable a) => HasServer (ReaderT env (ExceptT (Error a) IO)) where
  type ServerResult (ReaderT env (ExceptT (Error a) IO)) =
    (Error a -> Server IO) -> env -> IO (Server IO)

  renderServer server handleErr initEnv = fromReaderExcept handleErr initEnv server

fromReaderExcept ::
  (Show a, Typeable a) =>
  (Error a -> Server IO) ->
  env ->
  Server (ReaderT env (ExceptT (Error a) IO)) -> IO (Server IO)
fromReaderExcept handleErr env server =
  fmap (handleError handleErr) $
    flip runReaderT env $ ReaderT $
      \e -> pure $ hoistServer (either throw pure <=< runExceptT . flip runReaderT e) server

-- Prim types

-- | Type tag of Json-response.
data Json

-- server DSL

-- | Class ToServer contains anything convertible to @Server m@. We use it for flexuble composition
-- of servers from functions with arbitrary number of arguments. Arguments can
-- be of specific types: Query, Body, Optional, Capture, Header, etc.
-- We use type-level strings to encode query-names.
-- Example:
--
-- > "api" /. "foo" /.
-- >     (\(Query @"argA" argA) (Optional @"argB" argB) (Body jsonRequest) -> Post @Json $ handleFoo argA argB jsonRequest)
-- >
-- > handleFoo :: Int -> Maybe Text -> FooRequest -> IO FooResponse
-- > handleFoo = ...
--
-- Note that we can use any amount of arguments. And type of the input is decoded fron newtype wrapper
-- which is used with argument of the handler function.
--
-- Also we can return pure errors with Either. Anything which can be returned from function
-- can be wrapped to @Either (Error err)@.
--
-- For example in previous case we can use function which returns errors as values:
--
-- > type ServerError = Error Text
-- >
-- > handleFoo :: Int -> Maybe Text -> FooRequest -> IO (Either ServerError FooResponse)
-- > handleFoo = ...
--
-- the result of error response is automatically matched with normal response of the server
-- and standard Error type lets us pass status to response and some details.
class Monad (ServerMonad a) => ToServer a where
  type ServerMonad a :: (Type -> Type)
  toServer :: a -> Server (ServerMonad a)

instance Monad m => ToServer (Server m) where
  type ServerMonad (Server m) = m
  toServer = id

-- Status

-- | Set status to response. It can be ised inside any ToXxxResp value. Example:
--
-- > "api" /. handleFoo
-- >
-- > handleFoo :: Get Text IO (SetStatus Text)
-- > handleFoo = Get $ pure $ SetStatus status500 "Bad request"
data SetStatus a = SetStatus
  { status :: Status
  , content :: a
  }

-- | Sets status for response of the server
setStatus :: Monad m => Status -> Server m -> Server m
setStatus st = mapResp $ \resp -> resp { Resp.status = st }

-- Headers

-- | Attach headers to response. It can be used inside any ToXxxResp value.
-- Example:
--
-- > "api" /. handleFoo
-- >
-- > handleFoo :: Get Text IO (AddHeaders Text)
-- > handleFoo = Get $ pure $ AddHeaders headers "Hello foo"
data AddHeaders a = AddHeaders
  { headers :: ResponseHeaders
  , content :: a
  }

-- | Adds headers for response of the server
addHeaders :: Monad m => ResponseHeaders -> Server m -> Server m
addHeaders headers = mapResp $ \resp -> resp { Resp.headers = resp.headers <> headers }

mapResp :: Monad m => (Resp -> Resp) -> Server m -> Server m
mapResp f (Server act) = Server $ \req ->
  fmap (fmap f) $ act req

-- text response

-- | Values convertible to Text (lazy)
class ToTextResp a where
  toTextResp :: a -> Resp

instance ToTextResp Text where
  toTextResp = text

instance ToTextResp TL.Text where
  toTextResp = text

instance ToTextResp Int where
  toTextResp = text

instance ToTextResp a => ToTextResp (AddHeaders a) where
  toTextResp (AddHeaders headers content) =
    resp { Resp.headers = resp.headers <> headers }
    where
      resp = toTextResp content

instance ToTextResp a => ToTextResp (SetStatus a) where
  toTextResp (SetStatus st content) =
    setRespStatus st (toTextResp content)

instance (ToText err, ToTextResp a) => ToTextResp (Either (Error err) a) where
  toTextResp = either fromError toTextResp
    where
      fromError err = setRespStatus err.status (text err.body)

-- json response

-- | Values convertible to Json
class ToJsonResp a where
  toJsonResp :: a -> Resp

instance {-# OVERLAPPABLE #-} ToJSON a => ToJsonResp a where
  toJsonResp = json

instance ToJsonResp a => ToJsonResp (AddHeaders a) where
  toJsonResp (AddHeaders headers content) =
    resp { Resp.headers = resp.headers <> headers }
    where
      resp = toJsonResp content

instance ToJsonResp a => ToJsonResp (SetStatus a) where
  toJsonResp (SetStatus st content) =
    setRespStatus st (toJsonResp content)

instance (ToJSON err, ToJsonResp a) => ToJsonResp (Either (Error err) a) where
  toJsonResp = either fromError toJsonResp
    where
      fromError err = setRespStatus err.status (json err.body)

-- html response

-- | Values convertible to Html
class ToHtmlResp a where
  toHtmlResp :: a -> Resp

instance ToMarkup a => ToHtmlResp a where
  toHtmlResp = html

instance ToHtmlResp a => ToHtmlResp (AddHeaders a) where
  toHtmlResp (AddHeaders headers content) =
    resp { Resp.headers = resp.headers <> headers }
    where
      resp = toHtmlResp content

instance ToHtmlResp a => ToHtmlResp (SetStatus a) where
  toHtmlResp (SetStatus st content) =
    setRespStatus st (toHtmlResp content)

instance (ToJSON err, ToHtmlResp a) => ToHtmlResp (Either (Error err) a) where
  toHtmlResp = either fromError toHtmlResp
    where
      fromError err = setRespStatus err.status (json err.body)

-- Get

-- | Get method. Note that we can not use body input with Get-method, use Post for that.
-- So with Get we can use only URI inputs (Query, Optional, Capture)
newtype Get ty m a = Get (m a)

instance (Monad m, ToTextResp a) => ToServer (Get Text m a) where
  type ServerMonad (Get Text m a) = m
  toServer (Get act) = toMethod methodGet (toTextResp <$> act)

instance (Monad m, ToJSON a) => ToServer (Get Json m a) where
  type ServerMonad (Get Json m a) = m
  toServer (Get act) = toMethod methodGet (json <$> act)

instance (Monad m, ToHtmlResp a) => ToServer (Get Html m a) where
  type ServerMonad (Get Html m a) = m
  toServer (Get act) = toMethod methodGet (toHtmlResp <$> act)

instance (Monad m) => ToServer (Get BL.ByteString m BL.ByteString) where
  type ServerMonad (Get BL.ByteString m BL.ByteString) = m
  toServer (Get act) = toMethod methodGet (raw <$> act)

instance (Monad m) => ToServer (Get ByteString m ByteString) where
  type ServerMonad (Get ByteString m ByteString) = m
  toServer (Get act) = toMethod methodGet (raw . BL.fromStrict <$> act)

-- Post

-- | Post method
newtype Post ty m a = Post (m a)

instance (Monad m, ToTextResp a) => ToServer (Post Text m a) where
  type ServerMonad (Post Text m a) = m
  toServer (Post act) = toMethod methodPost $ toTextResp <$> act

instance (Monad m, ToJSON a) => ToServer (Post Json m a) where
  type ServerMonad (Post Json m a) = m
  toServer (Post act) = toMethod methodPost $ json <$> act

instance (Monad m, ToHtmlResp a) => ToServer (Post Html m a) where
  type ServerMonad (Post Html m a) = m
  toServer (Post act) = toMethod methodPost (toHtmlResp <$> act)

-- Put

-- | Put method
newtype Put ty m a = Put (m a)

instance (Monad m, ToTextResp a) => ToServer (Put Text m a) where
  type ServerMonad (Put Text m a) = m
  toServer (Put act) = toMethod methodPut $ toTextResp <$> act

instance (Monad m, ToJSON a) => ToServer (Put Json m a) where
  type ServerMonad (Put Json m a) = m
  toServer (Put act) = toMethod methodPut $ json <$> act

instance (Monad m, ToHtmlResp a) => ToServer (Put Html m a) where
  type ServerMonad (Put Html m a) = m
  toServer (Put act) = toMethod methodPut (toHtmlResp <$> act)

-- Delete

-- | Delete method
newtype Delete ty m a = Delete (m a)

instance (Monad m, ToTextResp a) => ToServer (Delete Text m a) where
  type ServerMonad (Delete Text m a) = m
  toServer (Delete act) = toMethod methodDelete $ toTextResp <$> act

instance (Monad m, ToJSON a) => ToServer (Delete Json m a) where
  type ServerMonad (Delete Json m a) = m
  toServer (Delete act) = toMethod methodDelete $ json <$> act

instance (Monad m, ToHtmlResp a) => ToServer (Delete Html m a) where
  type ServerMonad (Delete Html m a) = m
  toServer (Delete act) = toMethod methodDelete (toHtmlResp <$> act)

-- Patch

-- | Patch method
newtype Patch ty m a = Patch (m a)

instance (Monad m, ToTextResp a) => ToServer (Patch Text m a) where
  type ServerMonad (Patch Text m a) = m
  toServer (Patch act) = toMethod methodPatch $ toTextResp <$> act

instance (Monad m, ToJSON a) => ToServer (Patch Json m a) where
  type ServerMonad (Patch Json m a) = m
  toServer (Patch act) = toMethod methodPatch $ json <$> act

instance (Monad m, ToHtmlResp a) => ToServer (Patch Html m a) where
  type ServerMonad (Patch Html m a) = m
  toServer (Patch act) = toMethod methodPatch (toHtmlResp <$> act)

-- Options

-- | Options method
newtype Options ty m a = Options (m a)

instance (Monad m, ToTextResp a) => ToServer (Options Text m a) where
  type ServerMonad (Options Text m a) = m
  toServer (Options act) = toMethod methodOptions $ toTextResp <$> act

instance (Monad m, ToJSON a) => ToServer (Options Json m a) where
  type ServerMonad (Options Json m a) = m
  toServer (Options act) = toMethod methodOptions $ json <$> act

instance (Monad m, ToHtmlResp a) => ToServer (Options Html m a) where
  type ServerMonad (Options Html m a) = m
  toServer (Options act) = toMethod methodOptions (toHtmlResp <$> act)

-- Query

-- | Mandatary query parameter. Name is encoded as type-level string. Example:
--
-- > "api" /. handleFoo
-- >
-- > handleFoo :: Query "name" Int -> Server IO
-- > handleFoo (Query arg) = ...
newtype Query (sym :: Symbol) a = Query a

instance (FromHttpApiData a, ToServer b, KnownSymbol sym) => ToServer (Query sym a -> b) where
  type ServerMonad (Query sym a -> b) = ServerMonad b
  toServer act = withQuery (QueryName (Text.pack $ symbolVal (Proxy @sym))) (toServer . act . Query)

-- Optional query

-- | Optional query parameter. Name is encoded as type-level string. Example:
--
-- > "api" /. handleFoo
-- >
-- > handleFoo :: Optional "name" -> Server IO
-- > handleFoo (Optional maybeArg) = ...
newtype Optional (sym :: Symbol) a = Optional (Maybe a)

instance (FromHttpApiData a, ToServer b, KnownSymbol sym) => ToServer (Optional sym a -> b) where
  type ServerMonad (Optional sym a -> b) = ServerMonad b
  toServer act = withQuery' (QueryName (fromString $ symbolVal (Proxy @sym))) (toServer . act . Optional)

-- | Capture

-- Captures part of the path. Example
--
-- "api" /. "foo" /. (\(Capture n) -> handleFoo n)
--
-- It will parse the paths: "api/foo/358" and pass 358 to @handleFoo@.
newtype Capture a = Capture a

instance (FromHttpApiData a, ToServer b) => ToServer (Capture a -> b) where
  type ServerMonad (Capture a -> b) = ServerMonad b
  toServer act = toWithCapture $ \txt ->
    case parseUrlPiece txt of
      Right val -> toServer $ act $ Capture val
      Left err -> toConst $ pure $ badRequest ("Failed to parse capture: " <> err)

-- Read Body input

-- | Reads Json body (lazy). We can limit the body size with server config. Example:
--
-- > "api" /. "search" /. (\(Body request) -> handleSearch request)
newtype Body a = Body a

instance (MonadIO (ServerMonad b), FromJSON a, ToServer b) => ToServer (Body a -> b) where
  type ServerMonad (Body a -> b) = ServerMonad b
  toServer act = toWithBody $ \val ->
    case Json.eitherDecode val of
      Right v -> toServer $ act $ Body v
      Left err -> toConst $ pure $ badRequest $ "Failed to parse JSON body: " <> Text.pack err

-- | Reads raw body as lazy bytestring. We can limit the body size with server config. Example:
--
-- > "api" /. "upload" /. (\(RawBody content) -> handleUpload content)
newtype RawBody = RawBody BL.ByteString

instance (MonadIO (ServerMonad b), ToServer b) => ToServer (RawBody -> b) where
  type ServerMonad (RawBody -> b) = ServerMonad b
  toServer act = toWithBody $ toServer . act . RawBody

-- | Reads the URL encoded Form input
newtype FormBody a = FormBody a

instance (ToServer b, MonadIO (ServerMonad b), FromForm a) => ToServer (FormBody a -> b) where
  type ServerMonad (FormBody a -> b) = ServerMonad b
  toServer act = toWithFormData (toServer . act . FormBody)

-- Request Headers

-- | Reads input header. Example:
--
-- > "api" /. (\(Header @"Trace-Id" traceId) -> Post @Json (handleFoo traceId))
-- >
-- > handleFoo :: Maybe ByteString -> IO FooResponse
newtype Header (sym :: Symbol) a = Header (Maybe a)

instance (FromHttpApiData a, ToServer b, KnownSymbol sym) => ToServer (Header sym a -> b) where
  type ServerMonad (Header sym a -> b) = ServerMonad b
  toServer act = toWithHeader (fromString $ symbolVal (Proxy @sym)) (toServer . act . Header)

-- | Reads current path info
newtype PathInfo = PathInfo [Text]

instance (ToServer b) => ToServer (PathInfo -> b) where
  type ServerMonad (PathInfo -> b) = ServerMonad b
  toServer act = toWithPathInfo (toServer . act . PathInfo)

-- | Appends action to the server
withServerAction :: Monad m => Server m -> m () -> Server m
withServerAction srv act = Server $ \req -> do
  act
  unServer srv req

-------------------------------------------------------------------------------------
-- WAI

-- | Run server on port
runServer :: Int -> Server IO -> IO ()
runServer port server =
  Warp.run port (toApplication config server)
  where
    config = ServerConfig { maxBodySize = Nothing }

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Client library
module Mig.Client (
  ToClient (..),
  Client (..),
  ClientConfig (..),
  runClient,
  (:|) (..),
  FromClient (..),
  getRespOrValue,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.String
import Data.Text
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

-- | Synonym for pair
data (:|) a b = a :| b

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

class (MapRequest a) => ToClient a where
  -- | converts to client function
  toClient :: Server m -> a

  -- | how many routes client has
  clientArity :: Int

data ClientConfig = ClientConfig
  { port :: Int
  , manager :: Http.Manager
  }

newtype Client a = Client (ClientConfig -> CaptureMap -> Http.Request -> IO (RespOr AnyMedia Text a))
  deriving (Functor)

instance Applicative Client where
  pure a = Client $ const $ const $ const $ pure $ pureResp a
  (<*>) = ap

pureResp :: a -> RespOr AnyMedia Text a
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

runClient :: ClientConfig -> Client a -> IO (RespOr AnyMedia Text a)
runClient config (Client act) = act config mempty Http.defaultRequest

instance (IsMethod method, FromReqBody (RespMedia a) (RespBody a), IsResp a) => ToClient (Send method Client a) where
  toClient api =
    mapRequest (setRequestMethod (toMethod @method)) $
      Send $
        fmap ok $
          httpSend @(RespMedia a) @(RespBody a) (getHeadPath api)

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
addOptional (Optional mVal) req = case mVal of
  Nothing -> req
  Just val -> addQuery @sym (Query val) req

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
      toJson <$> Http.httpLbs (setRoute captureValues path $ setPort config.port req) config.manager
  where
    toJson resp = RespOr . bimap toResp toResp . fromReqBody @media $ Http.responseBody resp
      where
        toResp :: val -> Resp AnyMedia val
        toResp = Resp (Http.responseStatus resp) (Http.responseHeaders resp) . Just

setPort :: Int -> Http.Request -> Http.Request
setPort port req = req{Http.port = port}

setRoute :: CaptureMap -> Path -> Http.Request -> Http.Request
setRoute captureValues path req = req{Http.path = pathToString captureValues path}

----------------------------------------------------------
-- from response

class FromClient a where
  type ClientResult a :: Type
  fromClient :: a -> ClientConfig -> ClientResult a

instance FromClient (Send method Client (Resp media a)) where
  type ClientResult (Send method Client (Resp media a)) = IO (RespOr media Text a)
  fromClient = flip fromSendClient

instance (FromClient b) => FromClient (Body media a -> b) where
  type ClientResult (Body media a -> b) = a -> ClientResult b
  fromClient f config arg = fromClient (f (Body arg)) config

instance (FromClient b) => FromClient (Capture sym a -> b) where
  type ClientResult (Capture sym a -> b) = a -> ClientResult b
  fromClient f config arg = fromClient (f (Capture arg)) config

instance (FromClient b) => FromClient (Query sym a -> b) where
  type ClientResult (Query sym a -> b) = a -> ClientResult b
  fromClient f config arg = fromClient (f (Query arg)) config

instance (FromClient b) => FromClient (QueryFlag a -> b) where
  type ClientResult (QueryFlag a -> b) = Bool -> ClientResult b
  fromClient f config arg = fromClient (f (QueryFlag arg)) config

instance (FromClient b) => FromClient (Optional sym a -> b) where
  type ClientResult (Optional sym a -> b) = Maybe a -> ClientResult b
  fromClient f config arg = fromClient (f (Optional arg)) config

instance (FromClient b) => FromClient (Header sym a -> b) where
  type ClientResult (Header sym a -> b) = a -> ClientResult b
  fromClient f config arg = fromClient (f (Header arg)) config

instance (FromClient b) => FromClient (OptionalHeader sym a -> b) where
  type ClientResult (OptionalHeader sym a -> b) = Maybe a -> ClientResult b
  fromClient f config arg = fromClient (f (OptionalHeader arg)) config

instance (FromClient b) => FromClient (PathInfo -> b) where
  type ClientResult (PathInfo -> b) = ClientResult b
  fromClient f config = fromClient f config

instance (FromClient b) => FromClient (IsSecure -> b) where
  type ClientResult (IsSecure -> b) = ClientResult b
  fromClient f config = fromClient f config

instance (FromClient b) => FromClient (RawRequest -> b) where
  type ClientResult (RawRequest -> b) = ClientResult b
  fromClient f config = fromClient f config

instance (FromClient b) => FromClient (RawResponse -> b) where
  type ClientResult (RawResponse -> b) = ClientResult b
  fromClient f config = fromClient f config

fromSendClient :: ClientConfig -> Send method Client (Resp media a) -> IO (RespOr media Text a)
fromSendClient config (Send client) =
  joinRespOr <$> runClient config client

joinRespOr :: RespOr mediaA Text (Resp mediaB a) -> RespOr mediaC Text a
joinRespOr (RespOr eResp) = RespOr $ case eResp of
  Right resp -> case resp.body of
    Just respArg -> Right $ Resp resp.status resp.headers respArg.body
    Nothing -> Right $ Resp resp.status resp.headers Nothing
  Left resp -> Left $ Resp resp.status resp.headers resp.body

getRespOrValue :: RespOr media Text a -> Either Text a
getRespOrValue (RespOr eResp) = case eResp of
  Right resp -> maybe noContentValue Right resp.body
  Left resp -> maybe noContentValue Left resp.body
  where
    noContentValue = Left "No content in the response"

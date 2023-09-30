{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Client library
module Mig.Client (
  ToClient (..),
  Client (..),
  ClientConfig (..),
  runClient,
  (:|) (..),
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON (..), ToJSON)
import Data.Aeson qualified as Json
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.String
import Data.Text
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.TypeLits
import Mig.Core.Api
import Mig.Core.Route
import Mig.Core.Server
import Mig.Core.Types.MediaType (Json)
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Method
import Web.HttpApiData

data (:|) a b = a :| b

instance (ToClient a, ToClient b) => ToClient (a :| b) where
  toClient api = a :| b
    where
      (a, b) = toClient api

  clientArity = clientArity @(a, b)

class MapRequest a where
  mapRequest :: (Request -> Request) -> (a -> a)
  mapCapture :: (CaptureMap -> CaptureMap) -> (a -> a)

instance MapRequest (Send method Json Client a) where
  mapRequest f (Send (Client a)) = Send (Client (\conf capt -> a conf capt . f))
  mapCapture f (Send (Client a)) = Send (Client (\conf capt req -> a conf (f capt) req))

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
  { manager :: Manager
  }

newtype Client a = Client (ClientConfig -> CaptureMap -> Request -> IO (Either Text a))
  deriving (Functor)

instance Applicative Client where
  pure a = Client $ const $ const $ const $ pure $ pure a
  (<*>) = ap

instance Monad Client where
  (Client ma) >>= mf = Client $ \config captureValues req -> do
    eResp <- ma config captureValues req
    case eResp of
      Right resp -> case mf resp of
        Client run -> run config captureValues req
      Left err -> pure (Left err)

instance MonadIO Client where
  liftIO act = Client $ \_ _ _ -> Right <$> act

runClient :: ClientConfig -> Client a -> IO (Either Text a)
runClient config (Client act) = act config mempty defaultRequest

instance (IsMethod method, FromJSON a) => ToClient (Send method Json Client a) where
  toClient api = mapRequest (setMethod (toMethod @method)) $ Send $ httpJson (getHeadPath api)
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

setMethod :: Method -> Request -> Request
setMethod m req = req{method = m}

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Query sym a -> b) where
  toClient api = \query -> mapRequest (addQuery query) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Optional sym a -> b) where
  toClient api = \query -> mapRequest (addOptional query) $ toClient @b api
  clientArity = clientArity @b

instance (KnownSymbol sym, ToHttpApiData a, ToClient b) => ToClient (Capture sym a -> b) where
  toClient api = \capture -> mapCapture (addCapture capture) $ toClient @b api
  clientArity = clientArity @b

instance (ToJSON a, ToClient b) => ToClient (Body a -> b) where
  toClient api = \(Body body) -> mapRequest (addJsonBody body) $ toClient @b api
  clientArity = clientArity @b

{- TODO
instance (ToClient b) => ToClient (RawBody -> b) where
  toClient api = \(RawBody body) -> mapRequest (addRawBody body) $ toClient @b api
  clientArity = clientArity @b
-}

addQuery :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Query sym a -> Request -> Request
addQuery (Query a) req = req{queryString = str}
  where
    str =
      if B.null (queryString req)
        then param
        else queryString req <> "&" <> param

    param = fromString (symbolVal (Proxy @sym)) <> "=" <> Text.encodeUtf8 (toUrlPiece a)

addOptional :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Optional sym a -> Request -> Request
addOptional (Optional mVal) req = case mVal of
  Nothing -> req
  Just val -> addQuery @sym (Query val) req

addCapture :: forall sym a. (KnownSymbol sym, ToHttpApiData a) => Capture sym a -> CaptureMap -> CaptureMap
addCapture (Capture a) =
  Map.insert (fromString (symbolVal (Proxy @sym))) (toUrlPiece a)

addJsonBody :: (ToJSON a) => a -> Request -> Request
addJsonBody body req = req{requestBody = RequestBodyLBS (Json.encode body)}

{- TODO
addRawBody :: BL.ByteString -> Request -> Request
addRawBody body req = req{requestBody = RequestBodyLBS body}
-}

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

httpJson :: (FromJSON a) => Path -> Client a
httpJson path =
  Client $ \config captureValues req ->
    toJson <$> httpLbs (setRoute captureValues path req) config.manager
  where
    toJson = (first Text.pack . Json.eitherDecode) . responseBody

setRoute :: CaptureMap -> Path -> Request -> Request
setRoute captureValues path req = req{path = pathToString captureValues path}

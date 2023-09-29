-- | input implementation
module Mig.Core.ServerFun (
  ServerFun (..),
  MapServerFun (..),
  mapResp,
  sendResp,
  withBody,
  withRawBody,
  withQuery,
  withOptional,
  withCapture,
  withHeader,
  withOptionalHeader,
  withFormBody,
  withPathInfo,
  handleError,
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Mig.Core.Info
import Mig.Core.Types (
  Error (..),
  Req (..),
  Resp (..),
  badRequest,
  ok,
  setRespStatus,
 )
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status413)
import Web.FormUrlEncoded
import Web.HttpApiData

class MapServerFun f where
  mapServerFun :: (ServerFun m -> ServerFun n) -> f m -> f n

mapResp :: (Functor m, MapServerFun f) => (Resp -> Resp) -> f m -> f m
mapResp f = mapServerFun $ \(ServerFun fun) -> ServerFun (fmap (fmap f) . fun)

instance MapServerFun ServerFun where
  mapServerFun = id

instance ToRouteInfo (ServerFun m) where
  toRouteInfo = id

newtype ServerFun m = ServerFun {unServerFun :: Req -> m (Maybe Resp)}

withBody :: (MonadIO m, FromJSON a) => (a -> ServerFun m) -> ServerFun m
withBody f = withRawBody $ \val -> ServerFun $ \req ->
  case Json.eitherDecode val of
    Right v -> unServerFun (f v) req
    Left err -> pure $ Just $ badRequest $ "Failed to parse JSON body: " <> Text.pack err

withRawBody :: (MonadIO m) => (BL.ByteString -> ServerFun m) -> ServerFun m
withRawBody act = ServerFun $ \req -> do
  eBody <- liftIO req.readBody
  case eBody of
    Right body -> unServerFun (act body) req
    Left err -> pure $ Just $ setRespStatus err.status (ok @Text err.body)

withQuery :: (Monad m, FromHttpApiData a) => Text -> (a -> ServerFun m) -> ServerFun m
withQuery name act = withQueryBy (getQuery name) processResp
  where
    processResp = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse arg: " <> name

getQuery :: Text -> Req -> Maybe Text
getQuery name req = do
  bs <- Map.lookup (Text.encodeUtf8 name) req.query
  either (pure Nothing) Just $ Text.decodeUtf8' bs

handleMaybeInput :: (Applicative m) => Text -> (a -> ServerFun m) -> (Maybe a -> ServerFun m)
handleMaybeInput message act = \case
  Just arg -> ServerFun $ \req -> unServerFun (act arg) req
  Nothing -> ServerFun $ const $ pure $ Just $ badRequest message

withOptional :: (FromHttpApiData a) => Text -> (Maybe a -> ServerFun m) -> ServerFun m
withOptional name act = withQueryBy (getQuery name) act

withQueryBy ::
  (FromHttpApiData a) =>
  (Req -> Maybe Text) ->
  (Maybe a -> ServerFun m) ->
  ServerFun m
withQueryBy getVal act = ServerFun $ \req ->
  let
    -- TODO: do not ignore parse failure
    mArg = either (const Nothing) Just . parseQueryParam =<< getVal req
   in
    unServerFun (act mArg) req

withCapture :: (Monad m, FromHttpApiData a) => Text -> (a -> ServerFun m) -> ServerFun m
withCapture name act = withQueryBy getVal processResp
  where
    getVal req = Map.lookup name req.capture

    processResp = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse capture: " <> name

withHeader :: (Monad m, FromHttpApiData a) => HeaderName -> (a -> ServerFun m) -> ServerFun m
withHeader name act = withQueryBy getVal processResp
  where
    getVal req = eitherToMaybe . parseHeader =<< Map.lookup name req.headers

    processResp = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse header: " <> headerNameToText name

headerNameToText :: CI.CI ByteString -> Text
headerNameToText name = fromRight "" $ Text.decodeUtf8' $ CI.original name

withOptionalHeader :: (FromHttpApiData a) => HeaderName -> (Maybe a -> ServerFun m) -> ServerFun m
withOptionalHeader name act = withQueryBy getVal act
  where
    getVal req = eitherToMaybe . parseHeader =<< Map.lookup name req.headers

withFormBody :: (MonadIO m, FromForm a) => (a -> ServerFun m) -> ServerFun m
withFormBody act = withRawBody $ \body -> ServerFun $ \req -> do
  case urlDecodeForm body >>= fromForm of
    Right a -> unServerFun (act a) req
    Left err -> pure $ Just $ setRespStatus status413 $ badRequest err

withPathInfo :: ([Text] -> ServerFun m) -> ServerFun m
withPathInfo act = ServerFun $ \req -> unServerFun (act req.path) req

sendResp :: (Functor m) => m Resp -> ServerFun m
sendResp act = ServerFun $ const $ fmap Just act

-- | Handle errors
handleError :: (Exception a, MonadCatch m) => (a -> ServerFun m) -> ServerFun m -> ServerFun m
handleError handler (ServerFun act) = ServerFun $ \req ->
  (act req) `catch` (\err -> unServerFun (handler err) req)

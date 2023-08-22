-- | input implementation
module Mig.Internal.ServerFun (
  ServerFun (..),
  withBody,
  withRawBody,
  withQuery,
  withOptional,
  withCapture,
  withHeader,
  withFormBody,
  withPathInfo,
  sendText,
  sendJson,
  sendHtml,
  sendRaw,
  sendRawMedia,
) where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Mig.Internal.Info
import Mig.Internal.Types (
  Error (..),
  Req (..),
  Resp (..),
  ToByteStringResp (..),
  ToHtmlResp (..),
  ToJsonResp (..),
  ToTextResp (..),
  addRespHeaders,
  badRequest,
  setRespStatus,
  text,
 )
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status413)
import Web.FormUrlEncoded
import Web.HttpApiData

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
    Left err -> pure $ Just $ setRespStatus err.status (text err.body)

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

withHeader :: (Monad m, FromHttpApiData a) => HeaderName -> (Maybe a -> ServerFun m) -> ServerFun m
withHeader name act = ServerFun $ \req ->
  case Map.lookup name req.headers of
    Just bs ->
      case parseHeader bs of
        Right val -> unServerFun (act (Just val)) req
        Left err -> pure $ Just $ badRequest (errMessage err)
    Nothing -> unServerFun (act Nothing) req
  where
    errMessage :: Text -> Text
    errMessage err = "Failed to parse header " <> (fromRight "" $ Text.decodeUtf8' $ CI.original name) <> ": " <> err

withFormBody :: (MonadIO m, FromForm a) => (a -> ServerFun m) -> ServerFun m
withFormBody act = withRawBody $ \body -> ServerFun $ \req -> do
  case urlDecodeForm body >>= fromForm of
    Right a -> unServerFun (act a) req
    Left err -> pure $ Just $ setRespStatus status413 $ badRequest err

withPathInfo :: ([Text] -> ServerFun m) -> ServerFun m
withPathInfo act = ServerFun $ \req -> unServerFun (act req.path) req

sendText :: (Monad m, ToTextResp a) => m a -> ServerFun m
sendText act = ServerFun $ const $ fmap (Just . toTextResp) act

sendJson :: (Monad m, ToJsonResp a) => m a -> ServerFun m
sendJson act = ServerFun $ const $ fmap (Just . toJsonResp) act

sendHtml :: (Monad m, ToHtmlResp a) => m a -> ServerFun m
sendHtml act = ServerFun $ const $ fmap (Just . toHtmlResp) act

sendRaw :: (Monad m, ToByteStringResp a) => m a -> ServerFun m
sendRaw act = ServerFun $ const $ fmap (Just . toByteStringResp) act

sendRawMedia :: (Monad m, ToByteStringResp a) => MediaType -> m a -> ServerFun m
sendRawMedia (MediaType mediaType) act =
  ServerFun $ const $ fmap (Just . addRespHeaders [("Content-Type", Text.encodeUtf8 mediaType)] . toByteStringResp) act

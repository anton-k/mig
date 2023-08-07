-- | input implementation
module Mig.Internal.ServerFun
  ( ServerFun (..)
  , withBody
  , withRawBody
  , withQuery
  , withOptional
  , withCapture
  , withHeader
  , withFormBody
  , withPathInfo
  , sendText
  , sendJson
  , sendHtml
  , sendRaw
  ) where

import Control.Monad
import Data.Bifunctor
import Data.Text (Text)
import Data.Aeson (FromJSON)
import Web.HttpApiData
import Web.FormUrlEncoded
import Data.ByteString.Lazy qualified as BL
import Mig.Internal.Types
  ( Req (..), Resp (..),
   ToTextResp (..), ToJsonResp (..), ToHtmlResp (..), raw,
   Error (..), badRequest,
   setRespStatus
  )
import Mig.Internal.Info
import Network.HTTP.Types.Status (Status, ok200, status500, status413)
import Control.Monad.IO.Class
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as Text
import Data.List qualified as List
import Data.Either (fromRight)
import Network.HTTP.Types.Header (ResponseHeaders, RequestHeaders, HeaderName)
import Data.Text qualified as Text
import Data.Map.Strict qualified as Map

instance ToRouteInfo (ServerFun m) where
  toRouteInfo = id

newtype ServerFun m = ServerFun { unServerFun :: Req -> m (Maybe Resp) }

withBody :: FromJSON a => (a -> ServerFun m) -> ServerFun m
withBody = undefined

withRawBody :: (BL.ByteString -> ServerFun m) -> ServerFun m
withRawBody = undefined

withQuery :: FromHttpApiData a => Text -> (a -> ServerFun m) -> ServerFun m
withQuery = undefined

withOptional :: FromHttpApiData a => Text -> (Maybe a -> ServerFun m) -> ServerFun m
withOptional name act = ServerFun $ \req ->
  let
    mVal = Map.lookup (Text.encodeUtf8 name) req.query
    -- TODO: do not ignore parse failure
    mArg = either (const Nothing) Just . (parseQueryParam <=< first (Text.pack . show) . Text.decodeUtf8') =<< mVal
  in
    unServerFun (act mArg) req

withCapture :: FromHttpApiData a => Text -> (a -> ServerFun m) -> ServerFun m
withCapture = undefined

withHeader :: (Monad m, FromHttpApiData a) => HeaderName -> (Maybe a -> ServerFun m) -> ServerFun m
withHeader name act = ServerFun $ \req ->
  case fmap snd $ List.find ((== name) . fst) req.headers of
    Just bs ->
      case parseHeader bs of
        Right val -> unServerFun (act (Just val)) req
        Left err -> pure $ Just $ badRequest (errMessage err)
    Nothing -> unServerFun (act Nothing) req
  where
    errMessage :: Text -> Text
    errMessage err = "Failed to parse header " <> (fromRight "" $ Text.decodeUtf8' $ CI.original name) <> ": " <> err

withFormBody :: (MonadIO m, FromForm a) => (a -> ServerFun m) -> ServerFun m
withFormBody act = ServerFun $ \req -> do
  eBody <- first (\(Error _ details) -> details) <$> liftIO req.readBody
  case eBody >>= urlDecodeForm >>= fromForm of
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

sendRaw :: Monad m => m BL.ByteString -> ServerFun m
sendRaw act = ServerFun $ const $ fmap (Just . raw) act

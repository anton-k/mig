-- | input implementation
module Mig.Core.ServerFun (
  ServerFun (..),
  MapServerFun (..),
  mapResponse,
  sendResponse,
  withBody,
  withRawBody,
  withQuery,
  withQueryFlag,
  withOptional,
  withCapture,
  withHeader,
  withOptionalHeader,
  withFormBody,
  withPathInfo,
  handleError,
) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Mig.Core.Types
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status413, status500)
import Web.FormUrlEncoded
import Web.HttpApiData

class MapServerFun f where
  mapServerFun :: (ServerFun m -> ServerFun n) -> f m -> f n

mapResponse :: (Functor m, MapServerFun f) => (Response -> Response) -> f m -> f m
mapResponse f = mapServerFun $ \(ServerFun fun) -> ServerFun (fmap (fmap f) . fun)

instance MapServerFun ServerFun where
  mapServerFun = id

instance ToRouteInfo (ServerFun m) where
  toRouteInfo = id

newtype ServerFun m = ServerFun {unServerFun :: Request -> m (Maybe Response)}

withBody :: forall media a m. (MonadIO m, MimeUnrender media a) => (a -> ServerFun m) -> ServerFun m
withBody f = withRawBody $ \val -> ServerFun $ \req ->
  case mimeUnrender @media val of
    Right v -> unServerFun (f v) req
    Left err -> pure $ Just $ badRequest $ "Failed to parse request body: " <> err

withRawBody :: (MonadIO m) => (BL.ByteString -> ServerFun m) -> ServerFun m
withRawBody act = ServerFun $ \req -> do
  eBody <- liftIO req.readBody
  case eBody of
    Right body -> unServerFun (act body) req
    Left err -> pure $ Just $ setRespStatus status500 (ok @Text err)

withQuery :: (Monad m, FromHttpApiData a) => Text -> (a -> ServerFun m) -> ServerFun m
withQuery name act = withQueryBy (join . getQuery name) processResponse
  where
    processResponse = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse arg: " <> name

withQueryFlag :: Text -> (Bool -> ServerFun m) -> ServerFun m
withQueryFlag name act = ServerFun $ \req ->
  let
    val =
      case getQuery name req of
        Just (Just "") -> True
        Just (Just arg) ->
          case parseQueryParam @Bool arg of
            Right flag -> flag
            Left _ -> False
        Just Nothing -> True -- we interpret empty value as True for a flag
        Nothing -> False
   in
    unServerFun (act val) req

{-| The first maybe means that query with that name is missing
the second maybe is weather value is present or empty in the query
-}
getQuery :: Text -> Request -> Maybe (Maybe Text)
getQuery name req =
  case Map.lookup (Text.encodeUtf8 name) req.query of
    Just mBs ->
      case mBs of
        Just bs -> either (pure Nothing) (Just . Just) $ Text.decodeUtf8' bs
        Nothing -> Just Nothing
    Nothing -> Nothing

handleMaybeInput :: (Applicative m) => Text -> (a -> ServerFun m) -> (Maybe a -> ServerFun m)
handleMaybeInput message act = \case
  Just arg -> ServerFun $ \req -> unServerFun (act arg) req
  Nothing -> ServerFun $ const $ pure $ Just $ badRequest message

withOptional :: (FromHttpApiData a) => Text -> (Maybe a -> ServerFun m) -> ServerFun m
withOptional name act = withQueryBy (join . getQuery name) act

withQueryBy ::
  (FromHttpApiData a) =>
  (Request -> Maybe Text) ->
  (Maybe a -> ServerFun m) ->
  ServerFun m
withQueryBy getVal act = ServerFun $ \req ->
  let
    -- TODO: do not ignore parse failure
    mArg = either (const Nothing) Just . parseQueryParam =<< getVal req
   in
    unServerFun (act mArg) req

withCapture :: (Monad m, FromHttpApiData a) => Text -> (a -> ServerFun m) -> ServerFun m
withCapture name act = withQueryBy getVal processResponse
  where
    getVal req = Map.lookup name req.capture

    processResponse = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse capture: " <> name

withHeader :: (Monad m, FromHttpApiData a) => HeaderName -> (a -> ServerFun m) -> ServerFun m
withHeader name act = withQueryBy getVal processResponse
  where
    getVal req = eitherToMaybe . parseHeader =<< Map.lookup name req.headers

    processResponse = handleMaybeInput errorMessage act

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

sendResponse :: (Functor m) => m Response -> ServerFun m
sendResponse act = ServerFun $ const $ fmap Just act

-- | Handle errors
handleError :: (Exception a, MonadCatch m) => (a -> ServerFun m) -> ServerFun m -> ServerFun m
handleError handler (ServerFun act) = ServerFun $ \req ->
  (act req) `catch` (\err -> unServerFun (handler err) req)

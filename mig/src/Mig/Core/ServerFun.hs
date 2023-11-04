{-| Low-level server representarion.
The server is a function from @Request@ to @Response@.

> type ServerFun m = Request -> m (Maybe Response)

To use the mig library with some server library like wai/warp we need
to provide conversion of type @ServerFun@ to the representarion of the given library.
We can convert mig server to @ServerFun@ with function @fromServer@.
The @Maybe@ type in the result encodes missing routes.
-}
module Mig.Core.ServerFun (
  ServerFun,
  sendResponse,
  withBody,
  withRawBody,
  withQuery,
  withQueryFlag,
  withOptional,
  withCapture,
  withHeader,
  withOptionalHeader,
  withPathInfo,
  withFullPathInfo,
  handleServerError,
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
import Mig.Core.Class.MediaType
import Mig.Core.Types
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status (status500)
import Web.HttpApiData

{-| Low-level representation of the server.
Missing route for a given request returns @Nothing@.
-}
type ServerFun m = Request -> m (Maybe Response)

-- | Reads request body.
withBody :: forall media a m. (MonadIO m, FromReqBody media a) => (a -> ServerFun m) -> ServerFun m
withBody f = withRawBody $ \val -> \req ->
  case fromReqBody @media val of
    Right v -> f v req
    Left err -> pure $ Just $ badRequest @Text $ "Failed to parse request body: " <> err

-- | Reads low-level request body as byte string
withRawBody :: (MonadIO m) => (BL.ByteString -> ServerFun m) -> ServerFun m
withRawBody act = \req -> do
  eBody <- liftIO req.readBody
  case eBody of
    Right body -> act body req
    Left err -> pure $ Just $ setRespStatus status500 (okResponse @Text err)

-- | Reads required query parameter
withQuery :: (Monad m, FromHttpApiData a) => Text -> (a -> ServerFun m) -> ServerFun m
withQuery name act = withQueryBy (join . getQuery name) processResponse
  where
    processResponse = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse arg: " <> name

-- | Reads query flag
withQueryFlag :: Text -> (Bool -> ServerFun m) -> ServerFun m
withQueryFlag name act = \req ->
  let val =
        case getQuery name req of
          Just (Just "") -> True
          Just (Just arg) ->
            case parseQueryParam @Bool arg of
              Right flag -> flag
              Left _ -> False
          Just Nothing -> True -- we interpret empty value as True for a flag
          Nothing -> False
   in act val req

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
  Just arg -> \req -> act arg req
  Nothing -> const $ pure $ Just $ badRequest @Text message

-- | reads optional query parameter
withOptional :: (FromHttpApiData a) => Text -> (Maybe a -> ServerFun m) -> ServerFun m
withOptional name act = withQueryBy (join . getQuery name) act

-- | Generic query parameter reader
withQueryBy ::
  (FromHttpApiData a) =>
  (Request -> Maybe Text) ->
  (Maybe a -> ServerFun m) ->
  ServerFun m
withQueryBy getVal act = \req ->
  let -- TODO: do not ignore parse failure
      mArg = either (const Nothing) Just . parseQueryParam =<< getVal req
   in act mArg req

-- | Reads capture from the path
withCapture :: (Monad m, FromHttpApiData a) => Text -> (a -> ServerFun m) -> ServerFun m
withCapture name act = withQueryBy getVal processResponse
  where
    getVal req = Map.lookup name req.capture

    processResponse = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse capture: " <> name

-- | reads request header
withHeader :: (Monad m, FromHttpApiData a) => HeaderName -> (a -> ServerFun m) -> ServerFun m
withHeader name act = withQueryBy getVal processResponse
  where
    getVal req = eitherToMaybe . parseHeader =<< Map.lookup name req.headers

    processResponse = handleMaybeInput errorMessage act

    errorMessage = "Failed to parse header: " <> headerNameToText name

headerNameToText :: CI.CI ByteString -> Text
headerNameToText name = fromRight "" $ Text.decodeUtf8' $ CI.original name

-- | Reads optional request header
withOptionalHeader :: (FromHttpApiData a) => HeaderName -> (Maybe a -> ServerFun m) -> ServerFun m
withOptionalHeader name act = withQueryBy getVal act
  where
    getVal req = eitherToMaybe . parseHeader =<< Map.lookup name req.headers

-- | Reads full path (without qury parameters)
withPathInfo :: ([Text] -> ServerFun m) -> ServerFun m
withPathInfo act = \req -> act req.path req

-- | Reads full path (without qury parameters)
withFullPathInfo :: (Text -> ServerFun m) -> ServerFun m
withFullPathInfo act = \req -> act (toFullPath req) req

-- | Runs response getter action and returns it for any input request
sendResponse :: (Functor m) => m Response -> ServerFun m
sendResponse act = const $ fmap Just act

-- | Handle errors
handleServerError :: (Exception a, MonadCatch m) => (a -> ServerFun m) -> ServerFun m -> ServerFun m
handleServerError handler act = \req ->
  (act req) `catch` (\err -> handler err req)

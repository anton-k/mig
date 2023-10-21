{-| Debug utils for server. Simple logger for HTTP requests and responses
Also we can use real logging functions with ***By versions.
Simple variants are only for manual testing. It prints to stdout
with no ordering of the concurrent prints.

It can be useful for fast setup of debug for your application.
-}
module Mig.Extra.Plugin.Trace (
  logReq,
  logResp,
  logReqBy,
  logRespBy,
  logHttp,
  logHttpBy,
  ppReq,
  Verbosity (..),
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.Yaml qualified as Yaml
import Network.HTTP.Media.RenderHeader (renderHeader)
import Network.HTTP.Types.Status (Status (..))
import System.Time.Extra

import Mig.Core

-- | Verbosity level of echo prints
data Verbosity
  = -- | prints nothing
    V0
  | -- | prints time, path query, essential headers
    V1
  | -- | prints V1 + body
    V2
  | -- | prints V2 + all headers
    V3
  deriving (Eq, Ord, Show)

ifLevel :: Verbosity -> Verbosity -> [a] -> [a]
ifLevel current level vals
  | level <= current = vals
  | otherwise = []

-------------------------------------------------------------------------------------
-- through

-- | Logging of requests and responses
logHttp :: (MonadIO m) => Verbosity -> Plugin m
logHttp verbosity = logResp verbosity <> logReq verbosity

-- | Logging of requests and responses with custom logger
logHttpBy :: (MonadIO m) => (Json.Value -> m ()) -> Verbosity -> Plugin m
logHttpBy printer verbosity = logRespBy printer verbosity <> logReqBy printer verbosity

-------------------------------------------------------------------------------------
-- request

-- | Logs requests
logReq :: (MonadIO m) => Verbosity -> Plugin m
logReq = logReqBy defaultPrinter

-- | Logs requests with custom logger
logReqBy :: (MonadIO m) => (Json.Value -> m ()) -> Verbosity -> Plugin m
logReqBy printer verbosity = toPlugin $ \(RawRequest req) -> prependServerAction $ do
  when (verbosity > V0) $ do
    reqTrace <- liftIO $ do
      eBody <- req.readBody
      now <- getCurrentTime
      pure $ ppReq verbosity (Just now) eBody req
    printer reqTrace

-- | Pretty prints the request
ppReq :: Verbosity -> Maybe UTCTime -> Either Text BL.ByteString -> Request -> Json.Value
ppReq verbosity now body req =
  Json.object $
    concat $
      [ ifLevel verbosity V1 $
          mconcat
            [ maybe [] (pure . ("time" .=)) now
            ,
              [ "type" .= ("http-request" :: Text)
              , "path" .= toFullPath req
              , "method" .= Text.decodeUtf8 (renderHeader req.method)
              ]
            ]
      , ifLevel
          verbosity
          V2
          [ "body" .= fromBody body
          ]
      , ["headers" .= fromHeaders req.headers]
      ]
  where
    fromHeaders headers = Json.object $ fmap go $ onVerbosity $ (Map.toList headers)
      where
        go (name, val) =
          headerName name .= Text.decodeUtf8 val

        onVerbosity
          | verbosity < V3 = filter ((\name -> name == "Accept" || name == "Content-Type") . fst)
          | otherwise = id

    fromBody :: Either Text BL.ByteString -> Json.Value
    fromBody
      | isJsonReq = either Json.String jsonBody
      | otherwise = Json.String . either id (Text.decodeUtf8 . BL.toStrict)

    isJsonReq = Map.lookup "Content-Type" req.headers == Just "application/json"

-------------------------------------------------------------------------------------
-- response

-- | Logs response
logResp :: (MonadIO m) => Verbosity -> Plugin m
logResp = logRespBy defaultPrinter

-- | Logs response with custom logger
logRespBy :: forall m. (MonadIO m) => (Json.Value -> m ()) -> Verbosity -> Plugin m
logRespBy printer verbosity = toPlugin go
  where
    go :: PluginFun m
    go = \f -> \req -> do
      (dur, resp) <- duration (f req)
      when (verbosity > V0) $ do
        now <- liftIO getCurrentTime
        mapM_ (printer . ppResp verbosity now dur req) resp
      pure resp

-- | Pretty prints the response
ppResp :: Verbosity -> UTCTime -> Seconds -> Request -> Response -> Json.Value
ppResp verbosity now dur req resp =
  Json.object $
    concat
      [ ifLevel
          verbosity
          V1
          [ "time" .= now
          , "duration" .= dur
          , "type" .= ("http-response" :: Text)
          , "path" .= toFullPath req
          , "status" .= resp.status.statusCode
          , "method" .= Text.decodeUtf8 (renderHeader req.method)
          ]
      , ifLevel
          verbosity
          V2
          ["body" .= fromBody resp.body]
      , ["headers" .= fromHeaders resp.headers]
      ]
  where
    fromHeaders headers = Json.object $ fmap go $ headers
      where
        go (name, val) = headerName name .= Text.decodeUtf8 val

    fromBody = \case
      RawResp mediaType bs | mediaType == "application/json" -> jsonBody bs
      RawResp _ bs -> Json.String $ Text.decodeUtf8 (BL.toStrict bs)
      FileResp file -> Json.object ["file" .= file]
      StreamResp -> Json.object ["stream" .= ()]

-------------------------------------------------------------------------------------
-- utils

-- | Default printer
defaultPrinter :: (MonadIO m) => Json.Value -> m ()
defaultPrinter =
  liftIO . B.putStrLn . Yaml.encode . addLogPrefix

addLogPrefix :: Json.Value -> Json.Value
addLogPrefix val = Json.object ["log" .= val]

headerName :: CI ByteString -> Json.Key
headerName name = Json.fromText (Text.decodeUtf8 $ CI.foldedCase name)

jsonBody :: BL.ByteString -> Json.Value
jsonBody =
  either fromString id . Json.eitherDecode

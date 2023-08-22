module Mig.Internal.Wai (
  ServerConfig (..),
  Kilobytes,
  toApplication,
) where

import Control.Monad.Catch
import Data.Aeson qualified as Json
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Mig.Internal.ServerFun
import Mig.Internal.Types (Error (..), Req (..), Resp (..), RespBody (..), ToText (..), badRequest)
import Network.HTTP.Types.Status (status413)
import Network.Wai
import Text.Blaze.Renderer.Utf8 qualified as Html

-- | Size of the input body
type Kilobytes = Int

-- | Server config
data ServerConfig = ServerConfig
  { maxBodySize :: Maybe Kilobytes
  }

-- | Convert server to WAI-application
toApplication :: ServerConfig -> ServerFun IO -> Application
toApplication config server req processResponse = do
  mResp <- unServerFun (handleError onErr server) =<< fromRequest config.maxBodySize req
  processResponse $ toResponse $ fromMaybe noResult mResp
  where
    noResult = badRequest "Server produces nothing"

    onErr :: SomeException -> ServerFun IO
    onErr err = ServerFun $ const $ pure $ Just $ badRequest $ "Error: Exception has happened: " <> toText (show err)

-- | Convert response to low-level WAI-response
toResponse :: Resp -> Response
toResponse resp =
  case resp.body of
    TextResp textResp -> lbs $ BL.fromStrict (Text.encodeUtf8 textResp)
    HtmlResp htmlResp -> lbs (Html.renderMarkup htmlResp)
    JsonResp jsonResp -> lbs (Json.encode jsonResp)
    FileResp file -> responseFile resp.status resp.headers file Nothing
    RawResp str -> lbs str
    StreamResp -> undefined
  where
    lbs = responseLBS resp.status resp.headers

-- | Handle errors
handleError :: (Exception a, MonadCatch m) => (a -> ServerFun m) -> ServerFun m -> ServerFun m
handleError handler (ServerFun act) = ServerFun $ \req ->
  (act req) `catch` (\err -> unServerFun (handler err) req)

{-| Read request from low-level WAI-request
First argument limits the size of input body. The body is read in chunks.
-}
fromRequest :: Maybe Kilobytes -> Request -> IO Req
fromRequest maxSize req =
  pure $
    Req
      { path = pathInfo req
      , query = Map.fromList $ mapMaybe (\(key, mVal) -> (key,) <$> mVal) (queryString req)
      , headers = Map.fromList $ requestHeaders req
      , method = requestMethod req
      , readBody = fmap (fmap BL.fromChunks) $ readRequestBody (getRequestBodyChunk req) maxSize
      , capture = mempty
      }

-- | Read request body in chunks
readRequestBody :: IO B.ByteString -> Maybe Kilobytes -> IO (Either (Error Text) [B.ByteString])
readRequestBody readChunk maxSize = loop 0 Seq.empty
  where
    loop :: Kilobytes -> Seq B.ByteString -> IO (Either (Error Text) [B.ByteString])
    loop !currentSize !result
      | isBigger currentSize = pure outOfSize
      | otherwise = do
          chunk <- readChunk
          if B.null chunk
            then pure $ Right (toList result)
            else loop (currentSize + B.length chunk) (result |> chunk)

    outOfSize :: Either (Error Text) a
    outOfSize = Left (Error status413 (Text.pack $ "Request is too big Jim!"))

    isBigger = case maxSize of
      Just size -> \current -> current > size
      Nothing -> const False

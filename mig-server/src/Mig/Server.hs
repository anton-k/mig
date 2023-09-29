module Mig.Server (
  ServerConfig (..),
  toApplication,
  runServer,
  module X,
) where

import Mig.Core.Api as X (Api (..), Path (..), PathItem (..))
import Mig.Core.Info as X
import Mig.Core.Route as X
import Mig.Core.Server as X
import Mig.Core.Server.Class as X

import Control.Monad.Catch
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Mig.Core.ServerFun (handleError)
import Mig.Core.Types (Error (..), Req (..), Resp (..), RespBody (..), ToText (..), badRequest)
import Network.HTTP.Types.Status (status413)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

-- | Size of the input body
type Kilobytes = Int

-- | Server config
data ServerConfig = ServerConfig
  { maxBodySize :: Maybe Kilobytes
  }

runServer :: Int -> Server IO -> IO ()
runServer port server = Warp.run port (toApplication config server)
  where
    config = ServerConfig{maxBodySize = Nothing}

-- | Convert server to WAI-application
toApplication :: ServerConfig -> Server IO -> Wai.Application
toApplication config server req processResponse = do
  mResp <- unServerFun (handleError onErr (fromServer server)) =<< fromRequest config.maxBodySize req
  processResponse $ toResponse $ fromMaybe noResult mResp
  where
    noResult = badRequest "Server produces nothing"

    onErr :: SomeException -> ServerFun IO
    onErr err = ServerFun $ const $ pure $ Just $ badRequest $ "Error: Exception has happened: " <> toText (show err)

-- | Convert response to low-level WAI-response
toResponse :: Resp -> Wai.Response
toResponse resp =
  case resp.body of
    FileResp file -> Wai.responseFile resp.status resp.headers file Nothing
    RawResp _ str -> lbs str
    StreamResp -> undefined -- TODO
  where
    lbs = Wai.responseLBS resp.status resp.headers

{-| Read request from low-level WAI-request
First argument limits the size of input body. The body is read in chunks.
-}
fromRequest :: Maybe Kilobytes -> Wai.Request -> IO Req
fromRequest maxSize req =
  pure $
    Req
      { path = Wai.pathInfo req
      , query = Map.fromList $ mapMaybe (\(key, mVal) -> (key,) <$> mVal) (Wai.queryString req)
      , headers = Map.fromList $ Wai.requestHeaders req
      , method = Wai.requestMethod req
      , readBody = fmap (fmap BL.fromChunks) $ readRequestBody (Wai.getRequestBodyChunk req) maxSize
      , capture = mempty
      }

-- | Read request body in chunks
readRequestBody :: IO B.ByteString -> Maybe Kilobytes -> IO (Either Error [B.ByteString])
readRequestBody readChunk maxSize = loop 0 Seq.empty
  where
    loop :: Kilobytes -> Seq B.ByteString -> IO (Either Error [B.ByteString])
    loop !currentSize !result
      | isBigger currentSize = pure outOfSize
      | otherwise = do
          chunk <- readChunk
          if B.null chunk
            then pure $ Right (toList result)
            else loop (currentSize + B.length chunk) (result |> chunk)

    outOfSize :: Either Error a
    outOfSize = Left (Error status413 (Text.pack $ "Request is too big Jim!"))

    isBigger = case maxSize of
      Just size -> \current -> current > size
      Nothing -> const False

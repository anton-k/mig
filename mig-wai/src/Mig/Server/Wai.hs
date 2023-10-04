module Mig.Server.Wai (
  ServerConfig (..),
  Kilobytes,
  toApplication,
  runServer,
  module X,
) where

import Control.Monad.Catch
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Mig.Core.Api as X (Api (..), Path (..), PathItem (..))
import Mig.Core.Class.Route as X
import Mig.Core.Class.Server as X
import Mig.Core.Server as X
import Mig.Core.ServerFun (handleError)
import Mig.Core.Types (Request (..), Response (..), ResponseBody (..), ToText (..), badRequest)
import Mig.Core.Types.Info as X

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
  mResp <- handleError onErr (fromServer server) =<< fromRequest config.maxBodySize req
  processResponse $ toWaiResponse $ fromMaybe noResult mResp
  where
    noResult = badRequest @Text ("Server produces nothing" :: Text)

    onErr :: SomeException -> ServerFun IO
    onErr err = const $ pure $ Just $ badRequest @Text $ "Error: Exception has happened: " <> toText (show err)

-- | Convert response to low-level WAI-response
toWaiResponse :: Response -> Wai.Response
toWaiResponse resp =
  case resp.body of
    FileResp file -> Wai.responseFile resp.status resp.headers file Nothing
    RawResp _ str -> lbs str
    StreamResp -> undefined -- TODO
  where
    lbs = Wai.responseLBS resp.status resp.headers

{-| Read request from low-level WAI-request
First argument limits the size of input body. The body is read in chunks.
-}
fromRequest :: Maybe Kilobytes -> Wai.Request -> IO Request
fromRequest maxSize req = do
  bodyCache <- newBodyCache
  pure $
    Request
      { path = Wai.pathInfo req
      , query = Map.fromList (Wai.queryString req)
      , headers = Map.fromList $ Wai.requestHeaders req
      , method = Wai.requestMethod req
      , readBody = readBodyCache getBody bodyCache
      , capture = mempty
      , isSecure = Wai.isSecure req
      }
  where
    getBody =
      fmap (fmap BL.fromChunks) $ readRequestBody (Wai.getRequestBodyChunk req) maxSize

newtype BodyCache a = BodyCache (IORef (Maybe a))

newBodyCache :: IO (BodyCache a)
newBodyCache = BodyCache <$> newIORef Nothing

readBodyCache :: IO a -> BodyCache a -> IO a
readBodyCache getter (BodyCache ref) = do
  mVal <- readIORef ref
  case mVal of
    Just val -> pure val
    Nothing -> do
      val <- getter
      writeIORef ref (Just val)
      pure val

-- | Read request body in chunks. Note that this function can be used only once
readRequestBody :: IO B.ByteString -> Maybe Kilobytes -> IO (Either Text [B.ByteString])
readRequestBody readChunk maxSize = loop 0 Seq.empty
  where
    loop :: Kilobytes -> Seq B.ByteString -> IO (Either Text [B.ByteString])
    loop !currentSize !result
      | isBigger currentSize = pure outOfSize
      | otherwise = do
          chunk <- readChunk
          if B.null chunk
            then pure $ Right (toList result)
            else loop (currentSize + B.length chunk) (result |> chunk)

    outOfSize :: Either Text a
    outOfSize = Left "Request is too big Jim!"

    isBigger = case maxSize of
      Just size -> \current -> current > size
      Nothing -> const False

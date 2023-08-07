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

import Data.Text (Text)
import Data.Aeson (FromJSON)
import Web.HttpApiData
import Web.FormUrlEncoded
import Data.ByteString.Lazy qualified as BL
import Mig.Internal.Types (Req, Resp, ToTextResp, ToJsonResp, ToHtmlResp)
import Mig.Internal.Info

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
withOptional = undefined

withCapture :: FromHttpApiData a => Text -> (a -> ServerFun m) -> ServerFun m
withCapture = undefined

withHeader :: FromHttpApiData a => Text -> (a -> ServerFun m) -> ServerFun m
withHeader = undefined

withFormBody :: FromForm a => (a -> ServerFun m) -> ServerFun m
withFormBody = undefined

withPathInfo :: ([Text] -> ServerFun m) -> ServerFun m
withPathInfo = undefined

sendText :: (Monad m, ToTextResp a) => m a -> ServerFun m
sendText = undefined

sendJson :: (Monad m, ToJsonResp a) => m a -> ServerFun m
sendJson = undefined

sendHtml :: (Monad m, ToHtmlResp a) => m a -> ServerFun m
sendHtml = undefined

sendRaw :: m BL.ByteString -> ServerFun m
sendRaw = undefined

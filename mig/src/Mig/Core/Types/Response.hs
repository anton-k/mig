-- | Generic response
module Mig.Core.Types.Response (
  Resp (..),
  okResp,
  badResp,
  fromResp,
) where

import Data.OpenApi (ToSchema (..))
import Data.Typeable
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status, ok200)

import Mig.Core.Types.Http (Response, noContentResponse)
import Mig.Core.Types.Http qualified as Types

data Resp a = Resp
  { status :: Status
  , headers :: ResponseHeaders
  , body :: Maybe a
  }
  deriving (Show, Functor)

okResp :: a -> Resp a
okResp = Resp ok200 [] . Just

badResp :: Status -> a -> Resp a
badResp status = Resp status [] . Just

instance (ToSchema a) => ToSchema (Resp a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

fromResp :: (a -> Response) -> Resp a -> Response
fromResp f a =
  Types.setRespStatus a.status $ Types.addRespHeaders a.headers $ maybe (noContentResponse ok200) f a.body

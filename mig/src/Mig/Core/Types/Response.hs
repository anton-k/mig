-- | Generic response
module Mig.Core.Types.Response (
  Resp (..),
  okResp,
  badResp,
  addHeaders,
  setStatus,
  fromResp,
) where

import Data.OpenApi (ToSchema (..))
import Data.Typeable
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status, ok200)

import Mig.Core.Types.Http (Response)
import Mig.Core.Types.Http qualified as Types

data Resp a = Resp
  { status :: Status
  , headers :: ResponseHeaders
  , body :: a
  }
  deriving (Show, Functor)

okResp :: a -> Resp a
okResp = Resp ok200 []

badResp :: Status -> a -> Resp a
badResp status = Resp status []

addHeaders :: ResponseHeaders -> Resp a -> Resp a
addHeaders hs x = x{headers = x.headers <> hs}

setStatus :: Status -> Resp a -> Resp a
setStatus st x = x{status = st}

instance (ToSchema a) => ToSchema (Resp a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

fromResp :: (a -> Response) -> Resp a -> Response
fromResp f a = Types.setRespStatus a.status $ Types.addRespHeaders a.headers $ f a.body

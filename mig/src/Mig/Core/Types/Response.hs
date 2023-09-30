-- | Generic response
module Mig.Core.Types.Response (
  Response (..),
  okResponse,
  addHeaders,
  setStatus,
  fromResponse,
) where

import Data.OpenApi (ToSchema (..))
import Data.Typeable
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status, ok200)

import Mig.Core.Types.Http (Resp)
import Mig.Core.Types.Http qualified as Types

data Response a = Response
  { status :: Status
  , headers :: ResponseHeaders
  , body :: a
  }
  deriving (Show, Functor)

okResponse :: a -> Response a
okResponse = Response ok200 []

addHeaders :: ResponseHeaders -> Response a -> Response a
addHeaders hs x = x{headers = x.headers <> hs}

setStatus :: Status -> Response a -> Response a
setStatus st x = x{status = st}

instance (ToSchema a) => ToSchema (Response a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a)

fromResponse :: (a -> Resp) -> Response a -> Resp
fromResponse f a = Types.setRespStatus a.status $ Types.addRespHeaders a.headers $ f a.body

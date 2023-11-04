module Test.Server.Common (
  emptyReq,
  jsonResp,
) where

import Data.Aeson qualified as Json
import Data.Map.Strict qualified as Map
import Mig.Core
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (ok200)

emptyReq :: Request
emptyReq =
  Request
    { path = []
    , query = mempty
    , capture = mempty
    , headers = Map.fromList [("Accept", "application/json")]
    , method = methodGet
    , readBody = pure (Right "")
    , isSecure = False
    }

jsonResp :: (Json.ToJSON a) => a -> Response
jsonResp a =
  Response
    { status = ok200
    , headers = [("Content-Type", "application/json")]
    , body = RawResp "application/json" (Json.encode a)
    }

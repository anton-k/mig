{-# LANGUAGE UndecidableInstances #-}

-- | Core types and functions
module Mig.Core.Types (
  -- * types
  Req (..),
  Resp (..),
  RespBody (..),
  QueryMap,
  ToText (..),
  Error (..),
  fromError,

  -- * responses
  ok,
  badRequest,
  setContent,

  -- * utils
  setRespStatus,
  addRespHeaders,
) where

import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Mig.Core.Info (MediaType (..), MimeRender (..), ToMediaType (..))
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status, ok200, status500)

-- | Http response
data Resp = Resp
  { status :: Status
  -- ^ status
  , headers :: ResponseHeaders
  -- ^ headers
  , body :: RespBody
  -- ^ response body
  }

instance IsString Resp where
  fromString = ok @Text @Text . fromString

-- | Http response body
data RespBody
  = RawResp MediaType BL.ByteString
  | FileResp FilePath
  | StreamResp

-- | Http request
data Req = Req
  { path :: [Text]
  -- ^ URI path
  , query :: QueryMap
  -- ^ query parameters
  , capture :: CaptureMap
  -- ^ capture from path
  , headers :: HeaderMap
  -- ^ request headers
  , method :: Method
  -- ^ request method
  , readBody :: IO (Either Error BL.ByteString)
  -- ^ lazy body reader. Error can happen if size is too big (configured on running the server)
  }

type HeaderMap = Map HeaderName ByteString

type CaptureMap = Map Text Text

-- Errors

-- | Errors
data Error = Error
  { status :: Status
  , -- error status
    body :: Text
    -- message or error details
  }
  deriving (Show)

instance Exception Error

fromError :: (a -> Resp) -> Either Error a -> Resp
fromError f = \case
  Right a -> f a
  Left err -> setRespStatus err.status $ ok @Text err.body

-- | Map of query parameters for fast-access
type QueryMap = Map ByteString ByteString

-- | Bad request response
badRequest :: Text -> Resp
badRequest message = setRespStatus status500 $ ok @Text message

-- | Values convertible to lazy text
class ToText a where
  toText :: a -> Text

instance ToText TL.Text where
  toText = TL.toStrict

instance ToText Text where
  toText = id

instance ToText Int where
  toText = Text.pack . show

instance ToText Float where
  toText = Text.pack . show

instance ToText String where
  toText = fromString

{-# INLINE setContent #-}

-- | Headers to set content type
setContent :: MediaType -> ResponseHeaders
setContent (MediaType contentType) =
  [("Content-Type", Text.encodeUtf8 contentType <> "; charset=utf-8")]

-- | Sets response status
setRespStatus :: Status -> Resp -> Resp
setRespStatus status (Resp _ headers body) = Resp status headers body

addRespHeaders :: ResponseHeaders -> Resp -> Resp
addRespHeaders headers (Resp status hs body) = Resp status (headers <> hs) body

-- | Respond with ok 200-status
ok :: forall mime a. (MimeRender mime a) => a -> Resp
ok = Resp ok200 (setContent media) . RawResp media . mimeRender @mime
  where
    media = toMediaType @mime

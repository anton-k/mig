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
  Response (..),
  fromResponse,
  okResponse,
  addHeaders,
  setStatus,

  -- * classes
  ToTextResp (..),
  ToJsonResp (..),
  ToHtmlResp (..),
  ToByteStringResp (..),

  -- * responses
  text,
  json,
  html,
  raw,
  ok,
  badRequest,
  setContent,

  -- * utils
  setRespStatus,
  addRespHeaders,
) where

import Control.Monad.Catch
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.OpenApi (ToSchema (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Typeable
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status, ok200, status500)
import Text.Blaze.Html (Html, ToMarkup)
import Text.Blaze.Html qualified as Html

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
  fromString = text . TL.pack

-- | Values convertible to Text (lazy)
class ToTextResp a where
  toTextResp :: a -> Resp

instance ToTextResp Text where
  toTextResp = text

instance ToTextResp TL.Text where
  toTextResp = text

instance ToTextResp Int where
  toTextResp = text

-- | Values convertible to Json
class ToJsonResp a where
  toJsonResp :: a -> Resp

instance {-# OVERLAPPABLE #-} (ToJSON a) => ToJsonResp a where
  toJsonResp = json

-- | Values convertible to Html
class ToHtmlResp a where
  toHtmlResp :: a -> Resp

instance (ToMarkup a) => ToHtmlResp a where
  toHtmlResp = html

class ToByteStringResp a where
  toByteStringResp :: a -> Resp

instance ToByteStringResp BL.ByteString where
  toByteStringResp = raw

instance ToByteStringResp ByteString where
  toByteStringResp = raw . BL.fromStrict

instance ToByteStringResp Text where
  toByteStringResp = raw . BL.fromStrict . Text.encodeUtf8

-- | Http response body
data RespBody
  = TextResp Text
  | HtmlResp Html
  | JsonResp Json.Value
  | FileResp FilePath
  | StreamResp
  | RawResp BL.ByteString

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

-- Generic response

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
fromResponse f a = setRespStatus a.status $ addRespHeaders a.headers $ f a.body

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
  Left err -> setRespStatus err.status $ toTextResp err.body

-- | Map of query parameters for fast-access
type QueryMap = Map ByteString ByteString

-- | Bad request response
badRequest :: Text -> Resp
badRequest message =
  Resp
    { status = status500
    , headers = setContent "text/plain"
    , body = TextResp message
    }

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
setContent :: ByteString -> ResponseHeaders
setContent contentType =
  [("Content-Type", contentType <> "; charset=utf-8")]

-- | Sets response status
setRespStatus :: Status -> Resp -> Resp
setRespStatus status (Resp _ headers body) = Resp status headers body

addRespHeaders :: ResponseHeaders -> Resp -> Resp
addRespHeaders headers (Resp status hs body) = Resp status (headers <> hs) body

-- | Json response constructor
json :: (ToJSON resp) => resp -> Resp
json = (ok (setContent "application/json") . JsonResp . Json.toJSON)

-- | Text response constructor
text :: (ToText a) => a -> Resp
text = ok (setContent "text/plain") . TextResp . toText

-- | Html response constructor
html :: (ToMarkup a) => a -> Resp
html = ok (setContent "text/html") . HtmlResp . Html.toHtml

-- | Raw bytestring response constructor
raw :: BL.ByteString -> Resp
raw = ok [] . RawResp

-- | Respond with ok 200-status
ok :: ResponseHeaders -> RespBody -> Resp
ok headers body = Resp ok200 headers body

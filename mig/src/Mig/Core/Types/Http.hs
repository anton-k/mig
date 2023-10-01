{-# LANGUAGE UndecidableInstances #-}

-- | Core types and functions
module Mig.Core.Types.Http (
  -- * types
  Request (..),
  Response (..),
  RespBody (..),
  QueryMap,
  ToText (..),

  -- * responses
  ok,
  badRequest,
  setContent,

  -- * utils
  setRespStatus,
  addRespHeaders,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Mig.Core.Types.MediaType (MediaType, MimeRender (..), ToMediaType (..))
import Network.HTTP.Media.RenderHeader
import Network.HTTP.Types.Header (HeaderName, ResponseHeaders)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status, ok200, status500)

-- | Http response
data Response = Response
  { status :: Status
  -- ^ status
  , headers :: ResponseHeaders
  -- ^ headers
  , body :: RespBody
  -- ^ response body
  }

instance IsString Response where
  fromString = ok @Text @Text . fromString

-- | Http response body
data RespBody
  = RawResp MediaType BL.ByteString
  | FileResp FilePath
  | StreamResp

-- | Http request
data Request = Request
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
  , readBody :: IO (Either Text BL.ByteString)
  -- ^ lazy body reader. Error can happen if size is too big (configured on running the server)
  }

type HeaderMap = Map HeaderName ByteString

type CaptureMap = Map Text Text

-- | Map of query parameters for fast-access
type QueryMap = Map ByteString (Maybe ByteString)

-- | Bad request response
badRequest :: forall media a. (MimeRender media a) => a -> Response
badRequest message = setRespStatus status500 $ ok @media message

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
setContent media =
  [("Content-Type", renderHeader media)]

-- | Sets response status
setRespStatus :: Status -> Response -> Response
setRespStatus status (Response _ headers body) = Response status headers body

addRespHeaders :: ResponseHeaders -> Response -> Response
addRespHeaders headers (Response status hs body) = Response status (headers <> hs) body

-- | Respond with ok 200-status
ok :: forall mime a. (MimeRender mime a) => a -> Response
ok = Response ok200 (setContent media) . RawResp media . mimeRender @mime
  where
    media = toMediaType @mime

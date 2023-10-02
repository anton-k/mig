{-# LANGUAGE UndecidableInstances #-}

-- | Core types and functions
module Mig.Core.Types.Http (
  -- * types
  Request (..),
  Response (..),
  ResponseBody (..),
  QueryMap,
  ToText (..),

  -- * responses
  okResponse,
  badResponse,
  badRequest,
  setContent,
  noContentResponse,

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
import Mig.Core.Class.MediaType (MediaType, ToMediaType (..), ToRespBody (..))
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
  , body :: ResponseBody
  -- ^ response body
  }

noContentResponse :: Status -> Response
noContentResponse status = Response status [] (RawResp "*/*" "")

instance IsString Response where
  fromString = okResponse @Text @Text . fromString

-- | Http response body
data ResponseBody
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
badRequest :: forall media a. (ToRespBody media a) => a -> Response
badRequest message = badResponse @media status500 message

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
okResponse :: forall mime a. (ToRespBody mime a) => a -> Response
okResponse = Response ok200 (setContent media) . RawResp media . toRespBody @mime
  where
    media = toMediaType @mime

-- | Bad response qith given status
badResponse :: forall mime a. (ToRespBody mime a) => Status -> a -> Response
badResponse status = Response status (setContent media) . RawResp media . toRespBody @mime
  where
    media = toMediaType @mime

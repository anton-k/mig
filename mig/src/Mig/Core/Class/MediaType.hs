{-| Classes for MediaType and proper converters of Http values
from/to parameters or request/response bodies.
-}
module Mig.Core.Class.MediaType (
  MediaType,
  ToMediaType (..),
  ToRespBody (..),
  Json,
  FormUrlEncoded,
  OctetStream,
  AnyMedia,
  FromReqBody (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Json
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TextLazy
import Data.Text.Lazy.Encoding qualified as TextLazy
import Network.HTTP.Media.MediaType
import Text.Blaze.Html (Html, ToMarkup (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.FormUrlEncoded (FromForm, ToForm, urlDecodeAsForm, urlEncodeAsForm)

-- | Conversion of type-level tags to media type values
class ToMediaType a where
  toMediaType :: MediaType

instance ToMediaType Text where
  toMediaType = "text/plain"

instance ToMediaType Html where
  toMediaType = "text/html"

{-| Media type octet stream is for passing raw byte-strings in the request body.
It is converted to "application/octet-stream"
-}
data OctetStream

instance ToMediaType OctetStream where
  toMediaType = "application/octet-stream"

instance ToMediaType BL.ByteString where
  toMediaType = "application/octet-stream"

{-| Type-level tag for JSON media type
It is converted to "application/json"
-}
data Json

instance ToMediaType Json where
  toMediaType = "application/json"

{-| Type-level tag for FORM url encoded media-type.
It is converted to "application/x-www-form-urlencoded"
-}
data FormUrlEncoded

instance ToMediaType FormUrlEncoded where
  toMediaType = "application/x-www-form-urlencoded"

{-| Signifies any media. It prescribes the server renderer to lookup
media-type at run-time in the "Conten-Type" header.
As media-type it is rendered to "*/*".

It is useful for values for which we want to derive content type
from run-time values. For example it is used for static file servers
to get media type from file extension.
-}
data AnyMedia

instance ToMediaType AnyMedia where
  toMediaType = "*/*"

------------------------------------------------------------------------------------
-- mime render (everything that can be rendered as HTTP-output)

-- | Values that can be rendered to response body byte string.
class (ToMediaType ty) => ToRespBody ty b where
  toRespBody :: b -> BL.ByteString

instance (ToJSON a) => ToRespBody Json a where
  toRespBody = Json.encode

instance ToRespBody Text Text where
  toRespBody = BL.fromStrict . Text.encodeUtf8

instance ToRespBody Text TextLazy.Text where
  toRespBody = TextLazy.encodeUtf8

instance (ToMarkup a) => ToRespBody Html a where
  toRespBody = renderHtml . toMarkup

instance ToRespBody OctetStream BL.ByteString where
  toRespBody = id

instance ToRespBody OctetStream ByteString where
  toRespBody = BL.fromStrict

instance (ToForm a) => ToRespBody FormUrlEncoded a where
  toRespBody = urlEncodeAsForm

instance ToRespBody AnyMedia BL.ByteString where
  toRespBody = id

instance ToRespBody AnyMedia ByteString where
  toRespBody = BL.fromStrict

-------------------------------------------------------------------------------------
-- mime unrender (everything that can be parsed from HTTP-input)

-- | Values that can be parsed from request byte string.
class (ToMediaType ty) => FromReqBody ty b where
  fromReqBody :: BL.ByteString -> Either Text b

instance FromReqBody Text Text where
  fromReqBody = first (Text.pack . show) . Text.decodeUtf8' . BL.toStrict

instance FromReqBody OctetStream BL.ByteString where
  fromReqBody = Right

instance FromReqBody OctetStream ByteString where
  fromReqBody = Right . BL.toStrict

instance (FromJSON a) => FromReqBody Json a where
  fromReqBody = first Text.pack . Json.eitherDecode

instance (FromForm a) => FromReqBody FormUrlEncoded a where
  fromReqBody = urlDecodeAsForm

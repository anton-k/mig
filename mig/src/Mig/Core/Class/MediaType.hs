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
import Data.Aeson.Parser qualified
import Data.Aeson.Types (
  parseEither,
 )
import Data.Attoparsec.ByteString.Char8 (
  endOfInput,
  parseOnly,
  skipSpace,
  (<?>),
 )
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

class ToMediaType a where
  toMediaType :: MediaType

instance ToMediaType Text where
  toMediaType = "text/plain"

instance ToMediaType Html where
  toMediaType = "text/html"

data OctetStream

instance ToMediaType OctetStream where
  toMediaType = "application/octet-stream"

instance ToMediaType BL.ByteString where
  toMediaType = "application/octet-stream"

data Json

instance ToMediaType Json where
  toMediaType = "application/json"

data FormUrlEncoded

instance ToMediaType FormUrlEncoded where
  toMediaType = "application/x-www-form-urlencoded"

data AnyMedia

instance ToMediaType AnyMedia where
  toMediaType = "*/*"

------------------------------------------------------------------------------------
-- mime render (everything that can be rendered as HTTP-output)

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

class (ToMediaType ty) => FromReqBody ty b where
  fromReqBody :: BL.ByteString -> Either Text b

instance FromReqBody Text Text where
  fromReqBody = first (Text.pack . show) . Text.decodeUtf8' . BL.toStrict

instance FromReqBody OctetStream BL.ByteString where
  fromReqBody = Right

instance FromReqBody OctetStream ByteString where
  fromReqBody = Right . BL.toStrict

instance (FromJSON a) => FromReqBody Json a where
  fromReqBody = eitherDecodeLenient

instance (FromForm a) => FromReqBody FormUrlEncoded a where
  fromReqBody = urlDecodeAsForm

{-| Like 'Data.Aeson.eitherDecode' but allows all JSON values instead of just
objects and arrays.

Will handle trailing whitespace, but not trailing junk. ie.

>>> eitherDecodeLenient "1 " :: Either String Int
Right 1

>>> eitherDecodeLenient "1 junk" :: Either String Int
Left "trailing junk after valid JSON: endOfInput"
-}
eitherDecodeLenient :: (FromJSON a) => BL.ByteString -> Either Text a
eitherDecodeLenient input =
  first Text.pack $
    parseOnly parser (BL.toStrict input) >>= parseEither Json.parseJSON
  where
    parser =
      skipSpace
        *> Data.Aeson.Parser.value
        <* skipSpace
        <* (endOfInput <?> "trailing junk after valid JSON")

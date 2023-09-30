module Mig.Core.Types.MediaType (
  MediaType (..),
  ToMediaType (..),
  MimeRender (..),
  Json,
  Form,
  OctetStream,
) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BL
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.TypeLits
import Text.Blaze.Html (Html, ToMarkup (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

newtype MediaType = MediaType Text
  deriving (Show, Eq, Ord, IsString)

class ToMediaType a where
  toMediaType :: MediaType

instance ToMediaType Text where
  toMediaType = MediaType "text/plain"

instance ToMediaType Html where
  toMediaType = MediaType "text/html"

data OctetStream

instance ToMediaType OctetStream where
  toMediaType = MediaType "application/octet-stream"

instance ToMediaType BL.ByteString where
  toMediaType = MediaType "application/octet-stream"

data Json

instance ToMediaType Json where
  toMediaType = MediaType "application/json"

data RawMedia (sym :: Symbol)

data Form

instance ToMediaType Form where
  toMediaType = MediaType ""

instance (KnownSymbol sym) => ToMediaType (RawMedia sym) where
  toMediaType = MediaType (fromString (symbolVal (Proxy @sym)))

-------------------------------------------------------------------------------------
-- mime render (everything that can be rendered as HTTP-output)

class (ToMediaType ty) => MimeRender ty b where
  mimeRender :: b -> BL.ByteString

instance (ToJSON a) => MimeRender Json a where
  mimeRender = Json.encode

instance MimeRender Text Text where
  mimeRender = BL.fromStrict . Text.encodeUtf8

instance (ToMarkup a) => MimeRender Html a where
  mimeRender = renderHtml . toMarkup

instance MimeRender OctetStream BL.ByteString where
  mimeRender = id

instance MimeRender BL.ByteString BL.ByteString where
  mimeRender = id

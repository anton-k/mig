-- | Types that describe route info
module Mig.Core.Info (
  RouteInfo (..),
  RouteInput (..),
  Describe (..),
  noDescription,
  getInputType,
  RouteOutput (..),
  IsRequired (..),
  OutputSchema,
  InputSchema,
  SchemaDefs (..),
  emptySchemaDefs,
  toSchemaDefs,
  MediaType (..),
  addRouteInput,
  setOutputMedia,
  setMethod,
  setJsonMethod,
  emptyRouteInfo,
  ToMediaType (..),
  MimeRender (..),
  Json,
  RawMedia,
  ToRouteInfo (..),
  describeInfoInputs,
) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BL
import Data.List.Extra (firstJust)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.TypeLits
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Text.Blaze.Html (Html, ToMarkup (..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

data RouteInfo = RouteInfo
  { method :: Maybe Method
  , inputs :: [Describe RouteInput]
  , output :: RouteOutput
  , tags :: [Text]
  , description :: Text
  , summary :: Text
  }
  deriving (Show, Eq)

newtype IsRequired = IsRequired Bool
  deriving newtype (Show, Eq)

data Describe a = Describe
  { description :: Maybe Text
  , content :: a
  }
  deriving (Show, Eq)

noDescription :: a -> Describe a
noDescription = Describe Nothing

{-| Appends descriptiton for the info
special name request-body is dedicated to request body input
nd raw-input is dedicated to raw input
-}
describeInfoInputs :: [(Text, Text)] -> RouteInfo -> RouteInfo
describeInfoInputs descs routeInfo = routeInfo{inputs = fmap addDesc routeInfo.inputs}
  where
    addDesc inp =
      Describe (Map.lookup (getInputName inp) descMap) inp.content

    getInputName inp =
      case inp.content of
        ReqBodyInput _ _ -> "request-body"
        RawBodyInput -> "raw-input"
        CaptureInput captureName _ -> captureName
        QueryInput _ queryName _ -> queryName
        HeaderInput _ headerName _ -> headerName

    descMap = Map.fromList descs

data RouteInput
  = ReqBodyInput MediaType SchemaDefs
  | RawBodyInput
  | CaptureInput Text Schema
  | QueryInput IsRequired Text Schema
  | HeaderInput IsRequired Text Schema
  deriving (Show, Eq)

getInputType :: RouteInfo -> MediaType
getInputType route = fromMaybe (MediaType "*/*") $ firstJust (fromInput . (.content)) route.inputs
  where
    fromInput = \case
      ReqBodyInput ty _ -> Just ty
      _ -> Nothing

type InputSchema = SchemaDefs

data RouteOutput = RouteOutput
  { status :: Status
  , media :: MediaType
  , schema :: OutputSchema
  }
  deriving (Show, Eq)

type OutputSchema = SchemaDefs

data SchemaDefs = SchemaDefs
  { defs :: Definitions Schema
  , ref :: Maybe (Referenced Schema)
  }
  deriving (Show, Eq)

toSchemaDefs :: forall a. (ToSchema a) => SchemaDefs
toSchemaDefs =
  SchemaDefs defs (Just ref)
  where
    (defs, ref) = runDeclare (declareSchemaRef (Proxy @a)) mempty

emptySchemaDefs :: SchemaDefs
emptySchemaDefs = SchemaDefs mempty Nothing

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

instance MimeRender OctetStream BL.ByteString where
  mimeRender = id

instance MimeRender BL.ByteString BL.ByteString where
  mimeRender = id

class (ToMediaType ty) => MimeRender ty b where
  mimeRender :: b -> BL.ByteString

instance (ToJSON a) => MimeRender Json a where
  mimeRender = Json.encode

instance MimeRender Text Text where
  mimeRender = BL.fromStrict . Text.encodeUtf8

instance (ToMarkup a) => MimeRender Html a where
  mimeRender = renderHtml . toMarkup

data Json

instance ToMediaType Json where
  toMediaType = MediaType "application/json"

data RawMedia (sym :: Symbol)

data Form

instance ToMediaType Form where
  toMediaType = MediaType ""

instance (KnownSymbol sym) => ToMediaType (RawMedia sym) where
  toMediaType = MediaType (fromString (symbolVal (Proxy @sym)))

addRouteInput :: RouteInput -> RouteInfo -> RouteInfo
addRouteInput inp = addRouteInputWithDescriptiton (noDescription inp)

addRouteInputWithDescriptiton :: Describe RouteInput -> RouteInfo -> RouteInfo
addRouteInputWithDescriptiton inp routeInfo =
  routeInfo{inputs = inp : routeInfo.inputs}

emptyRouteInfo :: RouteInfo
emptyRouteInfo =
  RouteInfo Nothing [] (RouteOutput ok200 (MediaType "*/*") emptySchemaDefs) [] "" ""

setMethod :: Method -> MediaType -> RouteInfo -> RouteInfo
setMethod method mediaType routeInfo =
  routeInfo
    { method = Just method
    , output = RouteOutput routeInfo.output.status mediaType emptySchemaDefs
    }

setOutputMedia :: MediaType -> RouteInfo -> RouteInfo
setOutputMedia mediaType routeInfo =
  routeInfo{output = setMedia routeInfo.output}
  where
    setMedia outp = outp{media = mediaType}

setJsonMethod :: Method -> MediaType -> OutputSchema -> RouteInfo -> RouteInfo
setJsonMethod method mediaType apiSchema routeInfo =
  routeInfo
    { method = Just method
    , output = RouteOutput routeInfo.output.status mediaType apiSchema
    }

class ToRouteInfo a where
  toRouteInfo :: RouteInfo -> RouteInfo

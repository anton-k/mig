-- | Types that describe route info
module Mig.Core.Info (
  RouteInfo (..),
  RouteInput (..),
  RouteOutput (..),
  OutputSchema (..),
  emptyOutputSchema,
  toOutputSchema,
  toBodySchema,
  MediaType (..),
  addRouteInput,
  setMethod,
  setJsonMethod,
  setMediaInputType,
  emptyRouteInfo,
  ToMediaType (..),
  Json,
  RawMedia,
  ToRouteInfo (..),
) where

import Data.ByteString.Lazy qualified as BL
import Data.OpenApi
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy
import Data.String
import Data.Text (Text)
import GHC.TypeLits
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Text.Blaze.Html (Html)

data RouteInfo = RouteInfo
  { method :: Maybe Method
  , inputType :: MediaType
  , inputs :: [RouteInput]
  , output :: RouteOutput
  , tags :: [Text]
  , description :: Text
  , summary :: Text
  }
  deriving (Show, Eq)

data RouteInput
  = BodyJsonInput (Definitions Schema, Referenced Schema)
  | RawBodyInput
  | CaptureInput Text Schema
  | QueryInput Text Schema
  | OptionalInput Text Schema
  | HeaderInput Text Schema
  | FormBodyInput (Definitions Schema, Referenced Schema)
  deriving (Show, Eq)

toBodySchema :: (ToSchema a) => Proxy a -> (Definitions Schema, Referenced Schema)
toBodySchema proxy = runDeclare (declareSchemaRef proxy) mempty

data RouteOutput = RouteOutput
  { status :: Status
  , media :: MediaType
  , schema :: OutputSchema
  }
  deriving (Show, Eq)

data OutputSchema = OutputSchema
  { defs :: Definitions Schema
  , ref :: Maybe (Referenced Schema)
  }
  deriving (Show, Eq)

toOutputSchema :: (ToSchema a) => Proxy a -> OutputSchema
toOutputSchema proxy =
  OutputSchema defs (Just ref)
  where
    (defs, ref) = runDeclare (declareSchemaRef proxy) mempty

emptyOutputSchema :: OutputSchema
emptyOutputSchema = OutputSchema mempty Nothing

newtype MediaType = MediaType Text
  deriving (Show, Eq, Ord, IsString)

class ToMediaType a where
  toMediaType :: MediaType

instance ToMediaType Text where
  toMediaType = MediaType "text/plain"

instance ToMediaType Html where
  toMediaType = MediaType "text/html"

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

addRouteInput :: RouteInput -> RouteInfo -> RouteInfo
addRouteInput inp routeInfo =
  routeInfo{inputs = inp : routeInfo.inputs}

emptyRouteInfo :: RouteInfo
emptyRouteInfo =
  RouteInfo Nothing (MediaType "*/*") [] (RouteOutput ok200 (MediaType "*/*") emptyOutputSchema) [] "" ""

setMethod :: Method -> MediaType -> RouteInfo -> RouteInfo
setMethod method mediaType routeInfo =
  routeInfo
    { method = Just method
    , output = RouteOutput routeInfo.output.status mediaType emptyOutputSchema
    }

setJsonMethod :: Method -> MediaType -> OutputSchema -> RouteInfo -> RouteInfo
setJsonMethod method mediaType apiSchema routeInfo =
  routeInfo
    { method = Just method
    , output = RouteOutput routeInfo.output.status mediaType apiSchema
    }

setMediaInputType :: MediaType -> RouteInfo -> RouteInfo
setMediaInputType ty routeInfo = routeInfo{inputType = ty}

class ToRouteInfo a where
  toRouteInfo :: RouteInfo -> RouteInfo

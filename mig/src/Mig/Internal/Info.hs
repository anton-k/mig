-- | Types that describe route info
module Mig.Internal.Info (
  RouteInfo (..),
  RouteInput (..),
  RouteOutput (..),
  FormType (..),
  ToFormType (..),
  MediaType (..),
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
  = BodyJsonInput NamedSchema
  | RawBodyInput
  | CaptureInput Text Schema
  | QueryInput Text Schema
  | OptionalInput Text Schema
  | HeaderInput Text Schema
  | FormBodyInput FormType
  deriving (Show, Eq)

class ToFormType a where
  toFormType :: FormType

newtype FormType = FormType [(Text, Schema)]
  deriving (Show, Eq)

data RouteOutput = RouteOutput
  { status :: Status
  , media :: MediaType
  , schema :: Maybe NamedSchema
  }
  deriving (Show, Eq)

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

instance (KnownSymbol sym) => ToMediaType (RawMedia sym) where
  toMediaType = MediaType (fromString (symbolVal (Proxy @sym)))

addRouteInput :: RouteInput -> RouteInfo -> RouteInfo
addRouteInput inp info = info{inputs = inp : info.inputs}

emptyRouteInfo :: RouteInfo
emptyRouteInfo = RouteInfo Nothing (MediaType "*/*") [] (RouteOutput ok200 (MediaType "*/*") Nothing) [] "" ""

setMethod :: Method -> MediaType -> RouteInfo -> RouteInfo
setMethod method mediaType info =
  info
    { method = Just method
    , output = RouteOutput info.output.status mediaType Nothing
    }

setJsonMethod :: Method -> MediaType -> NamedSchema -> RouteInfo -> RouteInfo
setJsonMethod method mediaType schema info =
  info
    { method = Just method
    , output = RouteOutput info.output.status mediaType (Just schema)
    }

setMediaInputType :: MediaType -> RouteInfo -> RouteInfo
setMediaInputType ty info = info{inputType = ty}

class ToRouteInfo a where
  toRouteInfo :: RouteInfo -> RouteInfo

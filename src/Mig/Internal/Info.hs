-- | Types that describe route info
module Mig.Internal.Info
  ( RouteInfo (..)
  , RouteInput (..)
  , RouteOutput (..)
  , PrimType (..)
  , FormType (..)
  , ToFormType (..)
  , MediaInputType (..)
  , MediaType (..)
  , addRouteInput
  , setMethod
  , setMediaInputType
  , emptyRouteInfo
  , ToMediaType (..)
  , Json
  , ToPrimType (..)
  , ToJsonSpec (..)
  , ToRouteInfo (..)
  , JsonSpec (..)
  ) where

import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Data.ByteString.Lazy qualified as BL
import Text.Blaze.Html (Html)

class ToJsonSpec a where
  toJsonSpec :: JsonSpec

data RouteInfo = RouteInfo
  { method :: Maybe Method
  , inputType :: MediaInputType
  , inputs :: [RouteInput]
  , output :: RouteOutput
  }
  deriving (Show, Eq, Ord)

data RouteInput
  = BodyJsonInput JsonSpec
  | RawBodyInput
  | CaptureInput Text PrimType
  | QueryInput Text PrimType
  | OptionalInput Text PrimType
  | HeaderInput Text PrimType
  | FormBodyInput FormType
  deriving (Show, Eq, Ord)

data PrimType
  = IntType | TextType | DateType
  deriving (Show, Eq, Ord)

class ToPrimType a where
  toPrimType :: PrimType

instance ToPrimType Int where
  toPrimType = IntType

instance ToPrimType Text where
  toPrimType = TextType

class ToFormType a where
  toFormType :: FormType

newtype FormType = FormType [(Text, PrimType)]
  deriving (Show, Eq, Ord)

data JsonSpec = Any
  deriving (Show, Eq, Ord)

data RouteOutput = RouteOutput
  { status :: Status
  , media :: MediaType
  }
  deriving (Show, Eq, Ord)

data MediaInputType
  = JsonInputType | FormInputType | AnyInputType
  deriving (Show, Eq, Ord)

data MediaType
  = RawType | JsonType | HtmlType | PlainTextType | OtherMedia
  deriving (Show, Eq, Ord)

class ToMediaType a where
  toMediaType :: MediaType

instance ToMediaType Text where
  toMediaType = PlainTextType

instance ToMediaType Html where
  toMediaType = HtmlType

instance ToMediaType BL.ByteString where
  toMediaType = RawType

data Json

instance ToMediaType Json where
  toMediaType = JsonType

addRouteInput :: RouteInput -> RouteInfo -> RouteInfo
addRouteInput inp info = info { inputs = inp : info.inputs }

emptyRouteInfo :: RouteInfo
emptyRouteInfo = RouteInfo Nothing AnyInputType [] (RouteOutput ok200 PlainTextType)

setMethod :: Method -> MediaType -> RouteInfo -> RouteInfo
setMethod method mediaType info = info
  { method = Just method
  , output = RouteOutput info.output.status mediaType
  }

setMediaInputType :: MediaInputType -> RouteInfo -> RouteInfo
setMediaInputType ty info = info { inputType = ty }

class ToRouteInfo a where
  toRouteInfo :: RouteInfo -> RouteInfo


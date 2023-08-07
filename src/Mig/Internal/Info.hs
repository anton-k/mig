-- | Types that describe route info
module Mig.Internal.Info
  ( RouteInfo (..)
  , RouteInput (..)
  , RouteOutput (..)
  , PrimType (..)
  , FormType (..)
  , ToFormType (..)
  , MediaType (..)
  , addRouteInput
  , setMethod
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
  , inputs :: [RouteInput]
  , output :: RouteOutput
  }

data RouteInput
  = BodyJsonInput JsonSpec
  | RawBodyInput
  | CaptureInput Text PrimType
  | QueryInput Text PrimType
  | OptionalInput Text PrimType
  | HeaderInput Text PrimType
  | FormBodyInput FormType

data PrimType
  = IntType | TextType | DateType

class ToPrimType a where
  toPrimType :: PrimType

instance ToPrimType Int where
  toPrimType = IntType

class ToFormType a where
  toFormType :: FormType

newtype FormType = FormType [(Text, PrimType)]

data JsonSpec = Any

data RouteOutput = RouteOutput
  { status :: Status
  , media :: MediaType
  }

data MediaType
  = RawType | JsonType | HtmlType | PlainTextType

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
addRouteInput = undefined

emptyRouteInfo :: RouteInfo
emptyRouteInfo = RouteInfo Nothing [] (RouteOutput ok200 PlainTextType)

setMethod :: Method -> MediaType -> RouteInfo -> RouteInfo
setMethod = undefined

class ToRouteInfo a where
  toRouteInfo :: RouteInfo -> RouteInfo


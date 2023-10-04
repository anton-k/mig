-- | Types that describe route info
module Mig.Core.Types.Info (
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
  addRouteInput,
  setOutputMedia,
  setMethod,
  setJsonMethod,
  emptyRouteInfo,
  describeInfoInputs,

  -- * api updates
  addBodyInfo,
  addHeaderInfo,
  addOptionalHeaderInfo,
  addQueryInfo,
  addQueryFlagInfo,
  addOptionalInfo,
  addCaptureInfo,
) where

import Data.List.Extra (firstJust)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.OpenApi
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy
import Data.String
import Data.Text (Text)
import GHC.TypeLits
import Mig.Core.Class.MediaType
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

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
        QueryFlagInput queryName -> queryName
        HeaderInput _ headerName _ -> headerName

    descMap = Map.fromList descs

data RouteInput
  = ReqBodyInput MediaType SchemaDefs
  | RawBodyInput
  | CaptureInput Text Schema
  | QueryInput IsRequired Text Schema
  | QueryFlagInput Text
  | HeaderInput IsRequired Text Schema
  deriving (Show, Eq)

getInputType :: RouteInfo -> MediaType
getInputType route = fromMaybe "*/*" $ firstJust (fromInput . (.content)) route.inputs
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

addRouteInput :: RouteInput -> RouteInfo -> RouteInfo
addRouteInput inp = addRouteInputWithDescriptiton (noDescription inp)

addRouteInputWithDescriptiton :: Describe RouteInput -> RouteInfo -> RouteInfo
addRouteInputWithDescriptiton inp routeInfo =
  routeInfo{inputs = inp : routeInfo.inputs}

emptyRouteInfo :: RouteInfo
emptyRouteInfo =
  RouteInfo Nothing [] (RouteOutput ok200 "*/*" emptySchemaDefs) [] "" ""

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

addParamBy :: forall sym a. (KnownSymbol sym, ToParamSchema a) => (Text -> Schema -> RouteInput) -> RouteInfo -> RouteInfo
addParamBy cons = addRouteInput (cons (getName @sym) (toParamSchema (Proxy @a)))

addHeaderInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addHeaderInfo = addParamBy @sym @a (HeaderInput (IsRequired True))

addOptionalHeaderInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addOptionalHeaderInfo = addParamBy @sym @a (HeaderInput (IsRequired False))

addQueryInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addQueryInfo = addParamBy @sym @a (QueryInput (IsRequired True))

addOptionalInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addOptionalInfo = addParamBy @sym @a (QueryInput (IsRequired False))

addCaptureInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addCaptureInfo = addParamBy @sym @a CaptureInput

addQueryFlagInfo :: forall sym. (KnownSymbol sym) => RouteInfo -> RouteInfo
addQueryFlagInfo = addRouteInput (QueryFlagInput (getName @sym))

addBodyInfo :: forall ty a. (ToMediaType ty, ToSchema a) => RouteInfo -> RouteInfo
addBodyInfo = addRouteInput (ReqBodyInput (toMediaType @ty) (toSchemaDefs @a))

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))

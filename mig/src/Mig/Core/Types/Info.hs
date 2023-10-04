-- | Types that describe route info. We use it to derive OpenApi schema or clients.
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

-- | Information on route
data RouteInfo = RouteInfo
  { method :: Maybe Method
  -- ^ http method
  , inputs :: [Describe RouteInput]
  -- ^ route inputs
  , output :: RouteOutput
  -- ^ route outputs
  , tags :: [Text]
  -- ^ open-api tags
  , description :: Text
  -- ^ open-api description
  , summary :: Text
  -- ^ open-api summary
  }
  deriving (Show, Eq)

newtype IsRequired = IsRequired Bool
  deriving newtype (Show, Eq)

-- | Values which have human-readable description.
data Describe a = Describe
  { description :: Maybe Text
  , content :: a
  }
  deriving (Show, Eq)

-- | no description provided
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

-- | Route inputs
data RouteInput
  = ReqBodyInput MediaType SchemaDefs
  | RawBodyInput
  | CaptureInput Text Schema
  | QueryInput IsRequired Text Schema
  | QueryFlagInput Text
  | HeaderInput IsRequired Text Schema
  deriving (Show, Eq)

-- | Get input media-type
getInputType :: RouteInfo -> MediaType
getInputType route = fromMaybe "*/*" $ firstJust (fromInput . (.content)) route.inputs
  where
    fromInput = \case
      ReqBodyInput ty _ -> Just ty
      _ -> Nothing

-- | Input schema
type InputSchema = SchemaDefs

-- | Route output
data RouteOutput = RouteOutput
  { status :: Status
  -- ^ http status
  , media :: MediaType
  -- ^ media type
  , schema :: OutputSchema
  -- ^ open-api schema
  }
  deriving (Show, Eq)

-- | Output schema
type OutputSchema = SchemaDefs

-- | Schem definition with references to the used sub-values
data SchemaDefs = SchemaDefs
  { defs :: Definitions Schema
  , ref :: Maybe (Referenced Schema)
  }
  deriving (Show, Eq)

-- | Create schema definition
toSchemaDefs :: forall a. (ToSchema a) => SchemaDefs
toSchemaDefs =
  SchemaDefs defs (Just ref)
  where
    (defs, ref) = runDeclare (declareSchemaRef (Proxy @a)) mempty

-- | An empty schema definition
emptySchemaDefs :: SchemaDefs
emptySchemaDefs = SchemaDefs mempty Nothing

-- | Add route input to route info list of inputs
addRouteInput :: RouteInput -> RouteInfo -> RouteInfo
addRouteInput inp = addRouteInputWithDescriptiton (noDescription inp)

-- | Adds route input with description
addRouteInputWithDescriptiton :: Describe RouteInput -> RouteInfo -> RouteInfo
addRouteInputWithDescriptiton inp routeInfo =
  routeInfo{inputs = inp : routeInfo.inputs}

{-| Default empty route info. We update it as we construct the route with type-safe DSL.
Almost all values are derived from type signatures
-}
emptyRouteInfo :: RouteInfo
emptyRouteInfo =
  RouteInfo Nothing [] (RouteOutput ok200 "*/*" emptySchemaDefs) [] "" ""

-- | Set http-method of the route
setMethod :: Method -> MediaType -> RouteInfo -> RouteInfo
setMethod method mediaType routeInfo =
  routeInfo
    { method = Just method
    , output = RouteOutput routeInfo.output.status mediaType emptySchemaDefs
    }

-- | Set output meida-type for the route
setOutputMedia :: MediaType -> RouteInfo -> RouteInfo
setOutputMedia mediaType routeInfo =
  routeInfo{output = setMedia routeInfo.output}
  where
    setMedia outp = outp{media = mediaType}

-- | Add parameter to the inputs of the route
addParamInfoBy :: forall sym a. (KnownSymbol sym, ToParamSchema a) => (Text -> Schema -> RouteInput) -> RouteInfo -> RouteInfo
addParamInfoBy cons = addRouteInput (cons (getName @sym) (toParamSchema (Proxy @a)))

-- | Adds required header info to API schema
addHeaderInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addHeaderInfo = addParamInfoBy @sym @a (HeaderInput (IsRequired True))

-- | Adds optional header info to API schema
addOptionalHeaderInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addOptionalHeaderInfo = addParamInfoBy @sym @a (HeaderInput (IsRequired False))

-- | Adds required query info to API schema
addQueryInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addQueryInfo = addParamInfoBy @sym @a (QueryInput (IsRequired True))

-- | Adds optional query info to API schema
addOptionalInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addOptionalInfo = addParamInfoBy @sym @a (QueryInput (IsRequired False))

-- | Adds capture info to API schema
addCaptureInfo :: forall sym a. (KnownSymbol sym, ToParamSchema a) => RouteInfo -> RouteInfo
addCaptureInfo = addParamInfoBy @sym @a CaptureInput

-- | Adds query flag to API schema
addQueryFlagInfo :: forall sym. (KnownSymbol sym) => RouteInfo -> RouteInfo
addQueryFlagInfo = addRouteInput (QueryFlagInput (getName @sym))

-- | Adds request body to API schema
addBodyInfo :: forall ty a. (ToMediaType ty, ToSchema a) => RouteInfo -> RouteInfo
addBodyInfo = addRouteInput (ReqBodyInput (toMediaType @ty) (toSchemaDefs @a))

---------------------------------------------
-- utils

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))

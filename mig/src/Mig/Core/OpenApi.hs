module Mig.Core.OpenApi (
  toOpenApi,
) where

import Control.Lens ((%~), (&), (.~), (?~))
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.HashSet.InsOrd qualified as Set
import Data.Monoid (Endo (..))
import Data.OpenApi
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Mig.Core.Api (Api)
import Mig.Core.Api qualified as Api
import Mig.Core.Info qualified as Info
import Mig.Core.Route (Route (..))
import Mig.Core.Server (fillCaptures)
import Network.HTTP.Media.MediaType (MediaType)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (Status (..))

toOpenApi :: Api (Route m) -> OpenApi
toOpenApi x = case fillCaptures x of
  Api.Empty -> mempty
  Api.Append a b -> toOpenApi a <> toOpenApi b
  Api.WithPath (Api.Path path) a ->
    case path of
      [] -> toOpenApi a
      Api.StaticPath p : rest -> prependPath (Text.unpack p) $ toOpenApi $ Api.WithPath (Api.Path rest) a
      Api.CapturePath captureName : rest -> addCapture captureName $ toOpenApi $ Api.WithPath (Api.Path rest) a
  Api.Route a -> addPathItem a.api mempty
  where
    addCapture :: Text -> OpenApi -> OpenApi
    addCapture captureName =
      prependPath capture
        . addDefaultResponse404 tname
      where
        capture = "{" <> Text.unpack captureName <> "}"
        tname = captureName

addPathItem :: Info.RouteInfo -> OpenApi -> OpenApi
addPathItem routeInfo x = x{_openApiPaths = InsOrd.singleton mempty (toPathItem routeInfo)}

toPathItem :: Info.RouteInfo -> PathItem
toPathItem routeInfo =
  case routeInfo.method of
    Just method | method == methodGet -> mempty{_pathItemGet = Just op}
    Just method | method == methodPost -> mempty{_pathItemPost = Just op}
    Just method | method == methodPut -> mempty{_pathItemPut = Just op}
    Just method | method == methodDelete -> mempty{_pathItemDelete = Just op}
    Just method | method == methodOptions -> mempty{_pathItemOptions = Just op}
    Just method | method == methodHead -> mempty{_pathItemHead = Just op}
    Just method | method == methodPatch -> mempty{_pathItemPatch = Just op}
    Just method | method == methodTrace -> mempty{_pathItemTrace = Just op}
    _ -> mempty
  where
    op = toOperation routeInfo

toOperation :: Info.RouteInfo -> Operation
toOperation routeInfo =
  mempty
    & tags .~ Set.fromList routeInfo.tags
    & summary .~ nonEmptyText routeInfo.summary
    & description .~ nonEmptyText routeInfo.description
    & appEndo (foldMap (Endo . addInput) routeInfo.inputs)
    & addOutput routeInfo.output
  where
    addInput :: Info.RouteInput -> Operation -> Operation
    addInput = \case
      Info.QueryInput queryName querySchema ->
        addParam $
          mempty
            & name .~ queryName
            & required ?~ True
            & in_ .~ ParamQuery
            & schema ?~ Inline querySchema
      Info.OptionalInput queryName querySchema ->
        addParam $
          mempty
            & name .~ queryName
            & required ?~ False
            & in_ .~ ParamQuery
            & schema ?~ Inline querySchema
      Info.CaptureInput captureName captureSchema ->
        addParam $
          mempty
            & name .~ captureName
            & required ?~ True
            & in_ .~ ParamPath
            & schema ?~ Inline captureSchema
      Info.BodyJsonInput bodySchema -> addBody (fromString "application/json") (Just bodySchema._namedSchemaSchema)
      Info.RawBodyInput -> addBody (fromString "application/octet-stream") Nothing
      Info.FormBodyInput _ -> addBody (fromString "application/x-www-form-urlencoded") Nothing
      Info.HeaderInput headerName headerSchema ->
        addParam $
          mempty
            & name .~ headerName
            & required ?~ True
            & in_ .~ ParamHeader
            & schema ?~ Inline headerSchema

    addOutput :: Info.RouteOutput -> Operation -> Operation
    addOutput outp =
      responses
        .~ (mempty & responses .~ InsOrd.singleton outp.status.statusCode (Inline resp))
      where
        resp =
          mempty
            & content .~ InsOrd.singleton (toMediaType outp.media) (mempty & schema .~ fmap (Inline . (._namedSchemaSchema)) outp.schema)

toMediaType :: Info.MediaType -> MediaType
toMediaType (Info.MediaType txt) = fromString $ Text.unpack txt

nonEmptyText :: Text -> Maybe Text
nonEmptyText txt
  | Text.null txt = Nothing
  | otherwise = Just txt

-- | Add parameter to every operation in the spec.
addParam :: Param -> Operation -> Operation
addParam param = parameters %~ (Inline param :)

addBody :: MediaType -> Maybe Schema -> Operation -> Operation
addBody mediaType bodySchema =
  requestBody .~ Just (Inline body)
  where
    body =
      mempty
        & content .~ InsOrd.singleton mediaType (mempty & schema .~ (fmap Inline bodySchema))
        & required .~ Just True

addDefaultResponse404 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse404 pname = setResponseWith (\old _new -> alter404 old) 404 (return response404)
  where
    sname = markdownCode pname
    description404 = sname <> " not found"
    alter404 = description %~ ((sname <> " or ") <>)
    response404 = mempty & description .~ description404

-- | Format given text as inline code in Markdown.
markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"

module Mig.Core.OpenApi (
  toOpenApi,
) where

import Control.Lens (at, (%~), (&), (.~), (?~))
import Data.Aeson (ToJSON (..))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as Set
import Data.Monoid (Endo (..))
import Data.OpenApi hiding (Server (..))
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
import Mig.Core.Api (Api)
import Mig.Core.Api qualified as Api
import Mig.Core.Route (Route (..))
import Mig.Core.Server (Server (..), fillCaptures)
import Mig.Core.Types.Info (IsRequired (..), RouteInfo)
import Mig.Core.Types.Info qualified as Info
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (Status (..))

addCapture :: Text -> OpenApi -> OpenApi
addCapture captureName =
  prependPath ("{" <> Text.unpack captureName <> "}")

toOpenApi :: Server m -> OpenApi
toOpenApi (Server x) = fromApiInfo (fmap (.api) $ fillCaptures x)

fromApiInfo :: Api RouteInfo -> OpenApi
fromApiInfo = \case
  Api.Empty -> mempty
  Api.Append a b -> fromApiInfo a <> fromApiInfo b
  Api.WithPath (Api.Path path) a -> foldr withPath (fromApiInfo a) path
  Api.HandleRoute route -> fromRoute route

withPath :: Api.PathItem -> OpenApi -> OpenApi
withPath = \case
  Api.StaticPath pathName -> prependPath (Text.unpack pathName)
  Api.CapturePath captureName -> addCapture captureName

fromRoute :: RouteInfo -> OpenApi
fromRoute routeInfo =
  appEndo
    (foldMap (Endo . fromRouteInput) routeInfo.inputs)
    (fromRouteOutput routeInfo)

fromRouteOutput :: RouteInfo -> OpenApi
fromRouteOutput routeInfo =
  mempty
    & components . schemas .~ defs
    & paths . at "/"
      ?~ ( mempty
            & method
              ?~ ( mempty
                    & at code
                      ?~ Inline
                        ( mempty
                            & content
                              .~ InsOrdHashMap.fromList
                                [(t, mempty & schema .~ mref) | t <- responseContentTypes]
                            & headers .~ responseHeaders
                        )
                    & tags .~ Set.fromList routeInfo.tags
                    & summary .~ nonEmptyText routeInfo.summary
                    & description .~ nonEmptyText routeInfo.description
                 )
         )
  where
    method = case routeInfo.method of
      Just m | m == methodGet -> get
      Just m | m == methodPost -> post
      Just m | m == methodPut -> put
      Just m | m == methodDelete -> delete
      Just m | m == methodOptions -> options
      Just m | m == methodHead -> head_
      Just m | m == methodPatch -> patch
      Just m | m == methodTrace -> trace
      _ -> mempty

    code = routeInfo.output.status.statusCode

    responseContentTypes = [routeInfo.output.media]

    -- TODO: is it always empty?
    responseHeaders = Inline <$> mempty

    Info.SchemaDefs defs mref = routeInfo.output.schema

fromRouteInput :: Info.Describe Info.RouteInput -> OpenApi -> OpenApi
fromRouteInput descInput base = case descInput.content of
  Info.ReqBodyInput inputType bodySchema -> onRequestBody inputType bodySchema
  Info.RawBodyInput -> base
  Info.CaptureInput captureName captureSchema -> onCapture captureName captureSchema
  Info.QueryInput isRequired queryName querySchema -> onQuery isRequired queryName querySchema
  Info.QueryFlagInput queryName -> onQueryFlag queryName
  Info.HeaderInput isRequired headerName headerSchema -> onHeader isRequired headerName headerSchema
  where
    onCapture = onParam addDefaultResponse404 ParamPath (IsRequired True)

    onQuery = onParam addDefaultResponse400 ParamQuery

    onHeader = onParam addDefaultResponse400 ParamHeader

    onParam defResponse paramType (IsRequired isRequired) paramName paramSchema =
      base
        & addParam param
        & defResponse paramName
      where
        param =
          mempty
            & name .~ paramName
            & description .~ (nonEmptyText =<< descInput.description)
            & required ?~ isRequired
            & in_ .~ paramType
            & schema ?~ (Inline paramSchema)

    onRequestBody bodyInputType (Info.SchemaDefs defs ref) =
      base
        & addRequestBody reqBody
        & addDefaultResponse400 "body"
        & components . schemas %~ (<> defs)
      where
        reqBody =
          (mempty :: RequestBody)
            & description .~ (nonEmptyText =<< descInput.description)
            & content .~ InsOrdHashMap.fromList [(t, mempty & schema .~ ref) | t <- [bodyContentType]]

        bodyContentType = bodyInputType

    onQueryFlag queryName =
      base
        & addParam param
        & addDefaultResponse400 queryName
      where
        param =
          mempty
            & name .~ queryName
            & in_ .~ ParamQuery
            & allowEmptyValue ?~ True
            & schema
              ?~ ( Inline $
                    (toParamSchema (Proxy :: Proxy Bool))
                      & default_ ?~ toJSON False
                 )

-------------------------------------------------------------------------------------
-- openapi utils

nonEmptyText :: Text -> Maybe Text
nonEmptyText txt
  | Text.null txt = Nothing
  | otherwise = Just txt

-- | Add RequestBody to every operations in the spec.
addRequestBody :: RequestBody -> OpenApi -> OpenApi
addRequestBody rb = allOperations . requestBody ?~ Inline rb

-- | Add parameter to every operation in the spec.
addParam :: Param -> OpenApi -> OpenApi
addParam param = allOperations . parameters %~ (Inline param :)

addDefaultResponse404 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse404 pname = setResponseWith (\old _new -> alter404 old) 404 (return response404)
  where
    sname = markdownCode pname
    description404 = sname <> " not found"
    alter404 = description %~ ((sname <> " or ") <>)
    response404 = mempty & description .~ description404

addDefaultResponse400 :: ParamName -> OpenApi -> OpenApi
addDefaultResponse400 pname = setResponseWith (\old _new -> alter400 old) 400 (return response400)
  where
    sname = markdownCode pname
    description400 = "Invalid " <> sname
    alter400 = description %~ (<> (" or " <> sname))
    response400 = mempty & description .~ description400

-- | Format given text as inline code in Markdown.
markdownCode :: Text -> Text
markdownCode s = "`" <> s <> "`"

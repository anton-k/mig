module Mig.Core.OpenApi (
  toOpenApi,
) where

import Control.Lens (at, (%~), (&), (.~), (?~))
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as Set
import Data.OpenApi hiding (Server (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Mig.Core.Api (Api)
import Mig.Core.Api qualified as Api
import Mig.Core.Info (RouteInfo)
import Mig.Core.Info qualified as Info
import Mig.Core.Route (Route (..))
import Mig.Core.Server (Server (..), fillCaptures)
import Network.HTTP.Media.MediaType (MediaType)
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
  mconcat
    [ fromRouteOutput routeInfo
    , foldMap (fromRouteInput routeInfo.inputType) routeInfo.inputs
    ]

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

    responseContentTypes = [toMediaType routeInfo.output.media]

    -- TODO: is it always empty?
    responseHeaders = Inline <$> mempty

    Info.OutputSchema defs mref = routeInfo.output.schema

fromRouteInput :: Info.MediaType -> Info.RouteInput -> OpenApi
fromRouteInput inputType = \case
  Info.BodyJsonInput bodySchema -> onRequestBody inputType bodySchema
  Info.RawBodyInput -> undefined
  Info.CaptureInput captureName captureSchema -> onCapture captureName captureSchema
  Info.QueryInput queryName querySchema -> onQuery True queryName querySchema
  Info.OptionalInput queryName querySchema -> onQuery False queryName querySchema
  Info.HeaderInput headerName headerSchema -> onHeader headerName headerSchema
  Info.FormBodyInput formSchema -> onRequestBody inputType formSchema
  where
    onCapture = onParam addDefaultResponse404 ParamPath True

    onQuery = onParam addDefaultResponse400 ParamQuery

    onHeader = onParam addDefaultResponse400 ParamHeader True

    onParam defResponse paramType isRequired paramName paramSchema =
      mempty
        & addParam param
        & defResponse paramName
      where
        param =
          mempty
            & name .~ paramName
            -- & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
            & required ?~ isRequired
            & in_ .~ paramType
            & schema ?~ (Inline paramSchema)
    -- transDesc ""   = Nothing
    -- transDesc desc = Just (Text.pack desc)

    onRequestBody bodyInputType bodySchema =
      mempty
        & addRequestBody reqBody
        & addDefaultResponse400 tname
        & components . schemas %~ (<> defs)
      where
        tname = "body"
        -- transDesc ""   = Nothing
        -- transDesc desc = Just (Text.pack desc)
        (defs, ref) = bodySchema
        reqBody =
          (mempty :: RequestBody)
            -- & description .~ transDesc (reflectDescription (Proxy :: Proxy mods))
            & content .~ InsOrdHashMap.fromList [(t, mempty & schema ?~ ref) | t <- [bodyContentType]]

        bodyContentType = toMediaType bodyInputType

-------------------------------------------------------------------------------------
-- openapi utils

toMediaType :: Info.MediaType -> MediaType
toMediaType (Info.MediaType txt) = fromString $ Text.unpack txt

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

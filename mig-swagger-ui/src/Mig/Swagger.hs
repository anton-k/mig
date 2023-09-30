module Mig.Swagger (
  SwaggerConfig (..),
  withSwagger,

  -- * utils
  Default (..),
  DefaultInfo (..),
  addDefaultInfo,
  writeOpenApi,
  printOpenApi,
) where

import Control.Lens ((&), (.~), (?~))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Default
import Data.OpenApi (OpenApi)
import Data.OpenApi qualified as OA
import Data.Text (Text)
import Data.Text qualified as Text
import FileEmbedLzma
import Mig.Core.OpenApi
import Mig.Core.Types.MediaType (Json)
import Mig.Server
import Text.Blaze (ToMarkup (..))
import Text.Blaze.Html (Html)
import Web.HttpApiData

-- | Appends swagger UI to server
withSwagger :: (MonadIO m) => SwaggerConfig m -> Server m -> Server m
withSwagger config server =
  mconcat
    [ server
    , swagger config (pure openApi)
    ]
  where
    openApi = toOpenApi server

-- | Prints openapi schema file to stdout
printOpenApi :: Server m -> IO ()
printOpenApi server = BL.putStrLn $ encodePretty $ toOpenApi server

-- | Writes openapi schema to file
writeOpenApi :: FilePath -> Server m -> IO ()
writeOpenApi file server = BL.writeFile file $ encodePretty $ toOpenApi server

-- | Default info that is often added to OpenApi schema
data DefaultInfo = DefaultInfo
  { title :: Text
  , description :: Text
  , version :: Text
  }

addDefaultInfo :: DefaultInfo -> OpenApi -> OpenApi
addDefaultInfo appInfo =
  OA.info
    .~ ( mempty
          & OA.title .~ appInfo.title
          & OA.description ?~ appInfo.description
          & OA.version .~ appInfo.version
       )

instance Default DefaultInfo where
  def = DefaultInfo "" "" ""

-- | Swagger config
data SwaggerConfig m = SwaggerConfig
  { staticDir :: Path
  -- ^ path to server swagger (default is "/swagger-ui")
  , swaggerFile :: Path
  -- ^ swagger file name (default is "swaggger.json")
  , mapSchema :: OpenApi -> m OpenApi
  -- ^ apply transformation to OpenApi schema on serving OpenApi schema.
  -- it is useful to add additional info or set current date in the examples
  -- or apply any real-time transformation.
  }

instance (Applicative m) => Default (SwaggerConfig m) where
  def =
    SwaggerConfig
      { staticDir = "swagger-ui"
      , swaggerFile = "swagger.json"
      , mapSchema = pure
      }

-- | Swagger server. It serves static files and injects OpenApi schema
swagger :: forall m. (MonadIO m) => SwaggerConfig m -> m OpenApi -> Server m
swagger config getOpenApi =
  mconcat
    [ config.swaggerFile /. getSchema
    , config.staticDir
        /. mconcat
          [ "index.html" /. getIndex
          , staticFiles swaggerFiles
          , toServer getIndex
          ]
    ]
  where
    getSchema :: Get Json m Json.Value
    getSchema = Send $ Json.toJSON <$> (config.mapSchema =<< getOpenApi)

    getIndex :: Get Html m Html
    getIndex = Send $ do
      pure $
        preEscapedToMarkup $
          Text.replace "MIG_SWAGGER_UI_SCHEMA" (toUrlPiece config.swaggerFile) $
            Text.replace "MIG_SWAGGER_UI_DIR" (toUrlPiece config.staticDir) $
              indexTemplate

swaggerFiles :: [(FilePath, ByteString)]
swaggerFiles = $(embedRecursiveDir "swagger-ui-dist-5.0.0")

indexTemplate :: Text
indexTemplate = $(embedText "index.html.tmpl")

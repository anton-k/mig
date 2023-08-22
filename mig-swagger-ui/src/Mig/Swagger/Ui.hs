module Mig.Swagger.Ui
  ( SwaggerConfig (..)
  , withSwagger
  , swagger
  ) where

import Data.OpenApi (OpenApi)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Mig.Server
import Mig.OpenApi
import FileEmbedLzma
import Text.Blaze.Html (Html)
import Text.Blaze                     (ToMarkup (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson qualified as Json
import Web.HttpApiData
import Data.Default

-- | Appends swagger UI to server with path swagger-ui
withSwagger :: MonadIO m => SwaggerConfig m -> Server m -> Server m
withSwagger config server =
  mconcat
    [ server
    , swagger config (pure openApi)
    ]
  where
    openApi = toOpenApi server

data SwaggerConfig m = SwaggerConfig
  { staticDir :: Path
  , swaggerFile :: Path
  , mapSchema :: OpenApi -> m OpenApi
  }

instance Applicative m => Default (SwaggerConfig m) where
  def = SwaggerConfig
          { staticDir = "swagger-ui"
          , swaggerFile = "swagger.json"
          , mapSchema = pure
          }

swagger :: forall m . MonadIO m => SwaggerConfig m -> m OpenApi -> Server m
swagger config getOpenApi =
  mconcat
    [ config.swaggerFile /. route getSchema
    , config.staticDir /.
        mconcat
          [ "index.html" /. route getIndex
          , staticFiles swaggerFiles ""
          , route getIndex
          ]
    ]
  where
    getSchema :: Get Json m Json.Value
    getSchema = Send $ Json.toJSON <$> (config.mapSchema =<< getOpenApi)

    getIndex :: Get Html m Html
    getIndex = Send $ do
      pure $ preEscapedToMarkup
        $ Text.replace "MIG_SWAGGER_UI_SCHEMA" (toUrlPiece config.swaggerFile)
        $ Text.replace "MIG_SWAGGER_UI_DIR" (toUrlPiece config.staticDir)
        $ indexTemplate

swaggerFiles :: [(FilePath, ByteString)]
swaggerFiles = $(embedRecursiveDir "swagger-ui-dist-5.0.0")

indexTemplate :: Text
indexTemplate = $(embedText "index.html.tmpl")

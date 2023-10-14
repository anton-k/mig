module Server.Swagger (
  setSwagger,
) where

import Data.Text qualified as Text
import Mig
import Mig.Swagger

setSwagger :: Server IO -> Server IO
setSwagger = withSwagger config
  where
    config =
      (def :: SwaggerConfig IO)
        { mapSchema = pure . addDefaultInfo info
        }

    info =
      def
        { title = "Weather forecast"
        , description =
            Text.unlines
              [ "JSON API example for mig library which shows how to forecast weather to authorized users"
              , ""
              , "Registered users to get token: \"john\" with password \"123\" or \"mary\" with \"456\""
              , ""
              , "locations: \"moscow\", \"berlin\", \"sochi\", \"amsterdam\", \"oslo\", \"maykop\""
              ]
        , version = "0.1.0"
        }

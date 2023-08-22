-- | Most basic Hello world server.
-- It has only one route which outputs the greeting to the user
module Main
  ( main
  , hello'
  , bye'
  ) where

-- import Mig.Json.IO
import Data.Text (Text)
import Mig.Server
import Mig.Internal.Api (toNormalApi)
import Text.Show.Pretty
import Mig.OpenApi
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL
import Mig.Swagger.Ui
import Control.Lens ((.~), (?~), (&))

import Mig.Client
import Data.OpenApi qualified as OA

-- | We can render the server and run it on port 8085.
-- It uses wai and warp.
main :: IO ()
main = do
  putStrLn ("The hello world server listens on port: " <> show port)
  pPrint (fmap (.api) $ toNormalApi $ fillCaptures server)
  BL.putStrLn $ encodePretty $ toOpenApi server
  runServer port server
  where
    port = 8085

-- | Init simple hello world server which
-- replies on a single route
server :: Server IO
server = withSwagger swaggerConfig $
  "api" /. "v1" /. mconcat
    [ setDescription "Greeting action" $ "hello" /. "*" /. "*" /. route hello
    , "bye" /. route bye
    ]
  where
    swaggerConfig =
      SwaggerConfig
        { swaggerFile = "swagger.json"
        , staticDir = "swagger-ui"
        , mapSchema = pure . addInfo
        }

    addInfo =
      OA.info .~
        (mempty
          & OA.title .~ "Hello world app"
          & OA.description ?~ "Demo application"
          & OA.version .~ "1.0"
        )

type Hello m = Capture "who" Text -> Capture "suffix" Text -> Get Json m Text

-- | Handler takes no inputs and marked as Get HTTP-request that returns Text.
hello :: Hello IO
hello (Capture who) (Capture suffix) = Send $ pure $ mappend "Hello " who <> " " <> suffix

type Bye m = Optional "who" Text -> Post Json m Text

bye :: Bye IO
bye (Optional mWho) = Send $ pure $ mappend "Hello " $ maybe "World" id mWho

---------------------------------------------------------
-- client reuses server definition

hello' :: Hello Client
bye' :: Bye Client

hello'
 :| bye' = toClient server

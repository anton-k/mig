{-| Most basic Hello world server.
It has only one route which outputs the greeting to the user
-}
module Main (
  main,
  hello',
  bye',
) where

import Mig
import Mig.Core.Api (toNormalApi)
import Mig.Core.Server (fillCaptures)
import Mig.Swagger
import Text.Show.Pretty

import Mig.Client

{-| We can render the server and run it on port 8085.
It uses wai and warp.
-}
main :: IO ()
main = do
  putStrLn ("The hello world server listens on port: " <> show port)
  pPrint (fmap (.api) $ toNormalApi $ fillCaptures server.unServer)
  printOpenApi server
  runServer port (withSwagger swaggerConfig server)
  where
    port = 8085

    swaggerConfig =
      SwaggerConfig
        { swaggerFile = "swagger.json"
        , staticDir = "swagger-ui"
        , mapSchema = pure . addDefaultInfo info
        }

    info =
      DefaultInfo
        { title = "Hello world app"
        , description = "Demo application"
        , version = "1.0"
        }

{-| Init simple hello world server which
replies on a single route
-}
server :: Server IO
server =
  "api"
    /. "v1"
    /. mconcat
      [ helloDescription $ "hello" /. hello
      , "bye" /. bye
      ]
  where
    helloDescription =
      setDescription "Greeting action"
        . describeInputs
          [ ("who", "whom to greet")
          , ("suffix", "suffix to use on greeting")
          ]

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

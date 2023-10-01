{-| Most basic Hello world server.
It has only one route which outputs the greeting to the user
-}
module Main (
  main,
  hello',
  bye',
) where

import Data.Map.Strict qualified as Map
import Mig
import Mig.Core.Api (ApiNormal (..), MediaMap (..), OutputMediaMap (..), toNormalApi)
import Mig.Core.Server (fillCaptures)
import Mig.Core.Server.Trace qualified as Trace
import Mig.Swagger
import Network.HTTP.Types.Method (methodGet)
import Text.Show.Pretty

import Mig.Client

{-| We can render the server and run it on port 8085.
It uses wai and warp.
-}
main :: IO ()
main = do
  putStrLn ("The hello world server listens on port: " <> show port)
  pPrint
    ( case fmap (.api) $ toNormalApi $ fillCaptures (withSwagger swaggerConfig server).unServer of
        ApiNormal m ->
          case m Map.! methodGet of
            OutputMediaMap x -> map fst x.mapValues
    )
  --  printOpenApi server
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
  Trace.logHttp Trace.V1 $
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

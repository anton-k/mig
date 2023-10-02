{-| Most basic Hello world server.
It has only one route which outputs the greeting to the user
-}
module Main (
  main,
  -- hello',
  -- bye',
) where

import Mig
import Mig.Core.Trace qualified as Trace
import Mig.Swagger

-- import Mig.Client

{-| We can render the server and run it on port 8085.
It uses wai and warp.
-}
main :: IO ()
main = do
  putStrLn ("The hello world server listens on port: " <> show port)
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
  Trace.logHttp Trace.V2 $
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

type Hello m = Capture "who" Text -> Capture "suffix" Text -> Get m (Resp Json Text)

-- | Handler takes no inputs and marked as Get HTTP-request that returns Text.
hello :: Hello IO
hello (Capture who) (Capture suffix) =
  Send $
    pure $
      ok $
        "Hello " <> who <> " " <> suffix

type Bye m = Optional "who" Text -> Post m (Resp Json Text)

bye :: Bye IO
bye (Optional mWho) =
  Send $
    pure $
      ok $
        "Hello " <> maybe "World" id mWho

{-
---------------------------------------------------------
-- client reuses server definition

hello' :: Hello Client
bye' :: Bye Client
hello'
  :| bye' = toClient server
-}

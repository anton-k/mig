# JSON example: weather forecast

We have learned all we need to know about `mig` to be able to build something cool with it.
Let's build a weather forecast application. The app has registered users
which can request authroization tokens. With that token users can request for weather
in specific city and on specific time and also they can update the weather data.
For simplicity we omit user registration and defining roles for the user.

## Domain for our application

Let's define main types for our application in the module `Types.hs`.
We will import `Mig.Json.IO` to bring in scope some classes and types
common for HTTP-servers:

```haskell
module Types where

import Data.Time (Day)
import Mig.Json.IO
```

### Domain of users

There are users in the application that can register and get session tokens:

```haskell
data User = User
  { name :: Text
  , pass :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AuthToken = AuthToken Text
  deriving newtype 
    (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show, ToParamSchema, ToSchema)
```

We need instances to pass the dat over HTTP wires.

### Domain of weather

We are going to query weather info by location and date:

```haskell
newtype DayInterval = DayInterval Int
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema)

data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

deriving instance (ToSchema a) => ToSchema (Timed a)

newtype Location = Location Text
  deriving newtype 
    (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show, ToParamSchema, ToSchema)
```

The weather has information on temperature, speed of the wind, sun/rain nratio and pressure:

```haskell
data WeatherData = WeatherData
  { temperature :: Int
  , windSpeed :: Int
  , sunRainRatio :: Int
  , pressure :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
```

Also some users can update DB of weather:

```haskell
-- | Update weather data
data UpdateData = UpdateData
  { day :: Day
  , location :: Location
  , content :: WeatherData
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
```

That is our domain for the weather application.

## Lets define a server

We are going to build JSON HTTP application. For that we will use module `Mig.Json.IO`
which provides handy types specified to our domain.
We expect our application to have shared context `Env` which we pass to all handlers.

```haskell
import Mig.Json.IO
import Types

server :: Env -> Server IO
server env =
  withSwagger def $
    "api/v1/weather"
      /. mconcat
        [ auth
        , withAuth env $: app
        ]
  where
    auth = "get/auth-token" /. requestAuthToken env

    app =
      mconcat
        [ "get/weather" /. getWeather env
        , "update" /. updateWeather env
        ]

-- | Site internal shared context
data Env = Env

-- weather

getWeather ::
  Env ->
  Capture "location" Location ->
  Capture "day" Day ->
  Capture "day-interval" DayInterval ->
  Get (RespOr Text (Timed WeatherData))
getWeather = undefined

updateWeather ::
  Env ->
  Body UpdateData ->
  Post (RespOr Text ())
updateWeather = undefined

-- users

requestAuthToken :: Env -> Body User -> Post (RespOr Text AuthToken)
requestAuthToken = undefined

withAuth :: Env -> Header "auth" AuthToken -> Middleware IO
withAuth = undefined
```

we have one route to query for token:

```haskell
requestAuthToken :: Env -> Body User -> Post (RespOr Text AuthToken)
```

and two routes that query info on weather and update it:

```haskell
getWeather ::
  Env ->
  Capture "location" Location ->
  Capture "day" Day ->
  Capture "day-interval" DayInterval ->
  Get (RespOr Text (Timed WeatherData))

updateWeather ::
  Env ->
  Body UpdateData ->
  Post (RespOr Text ())
```

also we have a middleware that filters out non aunthorized calls:

```haskell
withAuth :: Env -> Header "auth" AuthToken -> Middleware IO
```

From its type-signature we can assume that authroization token
is passed in the header of the request.

## The structure of the server

We define server as a collection of actions that can be performed. 
The server is defined in terms of interfaces. We can initialize those interfaces
and pass them to handlers. 

Our app has several domains:

* users and sessions

* weather DB interface

* process lifecycle: logging, startup, cleanup, etc

So the server enironment has three parts:

```haskell
data Env = Env
  { auth :: Auth
  , weather :: Weather
  , proc :: Proc
  }
```

Let's define operations for those domains. We define them in the module `Interface.hs`.


### User domain

For the user we can do

* check that user is valid and can use the app

* allocate new authorization token

* check that token is valid

* expire the token (make it invalid)

```haskell
-- authorization interface
data Auth = Auth
  { newToken :: User -> IO AuthToken
  , validUser :: User -> IO Bool
  , validToken :: AuthToken -> IO Bool
  , expireToken :: AuthToken -> IO ()
  }
```

### Weather domain

For the weather we can query info and update it:

```haskell
-- weather forecast interface
data Weather = Weather
  { get :: Location -> Day -> DayInterval -> IO (Maybe (Timed WeatherData))
  , update :: UpdateData -> IO ()
  }
```

### Process domain

For the application process we keep all server lifecycle tools
which are not related to business logic domain. It can be logging, metrics,
startup and cleanup actions:

```haskell
-- | Process interface
data Proc = Proc
  { startup :: IO ()
  , cleanup :: IO ()
  , Logger :: Logger
  }

-- logger interface
data Logger = Logger
  { info :: LogFun
  , debug :: LogFun
  , error :: LogFun
  }

type LogFun = Value -> IO ()
```

We log JSON-values. As a helper functions we create funcitions
that can log anything which is convertible to JSON:

```haskell
logInfo :: (ToJSON a) => Env -> a -> IO ()
logInfo env = env.proc.logger.info . toJSON

logDebug :: (ToJSON a) => Env -> a -> IO ()
logDebug env = env.proc.logger.debug . toJSON

logError :: (ToJSON a) => Env -> a -> IO ()
logError env = env.proc.logger.error . toJSON
```

### Using interfacs

It's interesting to note how all actions on shared state can be expressed
as interfaces. We will declare the concrete mutable representation later
but for now it is ok to hide them with not implemented yet functions.
This also allows us to create mock applications for testing and substitute
implementations without changing the code for the server.

## Define server in terms of interfaces

As we have defined the main operations of the application we can complete 
the definition of the server.
Let's define the routes for weather domain first as they are more simple.

### Weather domain

We can query the weather forecast with function:

```haskell
getWeather ::
  Env ->
  Capture "location" Location ->
  Capture "day" Day ->
  Capture "day-interval" DayInterval ->
  Get (RespOr Text (Timed WeatherData))
getWeather env (Capture location) (Capture fromDay) (Capture interval) = Send $ do
  logInfo @Text env "get the weather forecast"
  mResult <- env.weather.get location fromDay interval
  pure $ case mResult of
    Just result -> ok result
    Nothing -> bad status400 "No data"
```

We log that call to get weather is in the progress.
Then we try to fetch weather data and if it has the data
we return it to the user otherwise we report error.

Let's update the weather data:

```haskell
updateWeather ::
  Env ->
  Body UpdateData ->
  Post (Resp ())
updateWeather env (Body updateData) = Send $ do
  logInfo @Text env "update the weather data"
  ok <$> env.weather.update updateData
```

we log the report and update the weather data.

### The user domain

Let's give the user access token and check that token is valid.
Let's allocate a new token in the hanlder `requestAuthToken`:

```haskell
requestAuthToken :: Env -> Body User -> Post (RespOr Text AuthToken)
requestAuthToken env (Body user) = Send $ do
  logInfo env ("get new auth token for: " <> user.name)
  isValid <- env.auth.validUser user
  if isValid
    then do
      token <- env.auth.newToken user
      void $ forkIO $ setExpireTimer token
      pure $ ok token
    else do
      logError env $ Text.unwords ["User", user.name, "does not have access to the service"]
      pure $ bad unauthorized401 "User is not valid"
  where
    setExpireTimer :: AuthToken -> IO ()
    setExpireTimer token = do
      threadDelay (1_000_000 * 60 * 10) -- 10 minutes
      env.auth.expireToken token
```

We check that user is valid and if the user is valid
we give user a token and also set the expiration for it. 
We will expire it 10 minutes after registration.
The expiration is concurrent process that is forked from the thread 
that hnadles the request. If user has no rights to use our service we report error.

Let's check for authorization tokens. Ideally we would like to add
this action to all handlers of our application. We would like to keep
the business logic handlers for the weather domain the same.
And we can do it with middleware. Let's define such a middleware
that expects authorization tokens with required header:

```haskell
withAuth :: Env -> Header "auth" AuthToken -> Middleware IO
withAuth env (Header token) = processResponse $ \getResp -> do
  isOk <- env.auth.validToken token
  if isOk
    then getResp
    else do
      logError env errMessage
      pure $ Just (bad status500 $ Text.encodeUtf8 errMessage)
  where
    errMessage = "Token is invalid"
```

we have covered in depth how to implement it in the chapter on Middlewares
so this code should look familiar to us.


### Run application

That completes the definition of the server. Let's run it. We define the main function
in the module `Main.hs`:

```haskell
main :: IO ()
main = do
  env <- initEnv port
  env.proc.startup
  runServer port (server env)
    `finally` env.proc.cleanup
  where
    port = 8085

initEnv :: Port -> IO Env
initEnv = undefined
```

The initialization of interfaces is yet to be defined.
So this is all we need to start the server.

## Implementation of the interfaces

For the purpose of the example we will create a mock application.
A bit more detailed implementation is in the source code of the `mig` library.
See example `JsonApi`.


## Mock application

We can create a mock application with one user.

```haskell
import Data.ByteString.Char8 qualified as B
import Data.Yaml qualified as Yaml
import Data.Aeson qualified as Json

initEnv :: Port -> IO Env
initEnv port = pure $ Env initAuth initWeather (initProc proc)

-- | Application with single user john with token that never expires
initAuth :: Auth
initAuth = 
  Auth
  { newToken = const $ pure AuthToken "john-token"
  , validUser = \(User name pass) = pure $ name == "john" && pass == "123"
  , validToken = (\AuthToken token) -> pure (token == "john-token")
  , expireToken = const $ pure ()
  }

initWeather = Weather
  { get = \location day dayInterval -> pure Nothing
  , update = \updateData -> pure ()
  }

initProc :: Port -> Proc
initProc = 
  pure Proc 
    { logger = logger
    , startup = logger.info $ "App started on port: " <> Text.pack (show port)
    , cleanup = logger.info "App shutdown"
    }
  where
    logger = initLogger

initLogger :: Logger
initLogger = Logger
  { info = logBy "info" 
  , debug = logBy "debug"
  , error = logBy "error"
  }
  where
    logBy :: Text -> Json.Value msg -> IO ()
    logBy level msg = B.putStrLn . Yaml.encode . addLogPrefix $
        Json.object [ "level" .= level, "message" .= msg ]

addLogPrefix :: Json.Value -> Json.Value
addLogPrefix val = Json.object ["log" .= val]
```

we can start the application and try it out with swagger.

## Exercises

You can find the complete code of the example in the [`mig` repo](https://github.com/anton-k/mig/blob/main/examples/mig-example-apps/JsonApi).

* implement routes for user registration. Only registered users can receive the authorization token.

* implement roles for the users:
    * admin: can manage users
    * db-writer: can update weather forecast
    * visitor: can only view forecasts

* implement in-memory storage of the weather DB. Use maps to store weather data and info
  on valid users and tokens.

* implement real logger with `fast-logger` library

* implement interface that connects application to some real DB.
  The code for the server should stay the same and only initialization
  of interface should change. Use one of the DB libraries for haskell: `hasql`, `postgresql-simple`

## Summary 

In this chapter we have defined a more substantial example of JSON HTTP application
and saw how we can apply various concepts in practice.

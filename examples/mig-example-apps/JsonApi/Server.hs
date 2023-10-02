-- | server and handlers
module Server (
  server,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Text qualified as Text
import Data.Time
import Mig.Json.IO
import Mig.Swagger

import Interface
import Mig.Core.Trace qualified as Trace
import Types

server :: Env -> Server IO
server env =
  withTrace $
    withSwagger config $
      "api/v1/weather"
        /. mconcat
          [ "get"
              /. mconcat
                [ "weather/*/*/*" /. handleGetWeather env
                , "auth-token" /. handleAuthToken env
                ]
          , "update" /. handleUpdateWeather env
          ]
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

    withTrace = Trace.logHttpBy (logInfo env) Trace.V2

handleAuthToken :: Env -> Body User -> Post (RespOr Text AuthToken)
handleAuthToken env (Body user) = Send $ do
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

handleGetWeather ::
  Env ->
  Header "auth" AuthToken ->
  Capture "location" Location ->
  Capture "day" Day ->
  Capture "day-interval" DayInterval ->
  Get (RespOr Text (Timed WeatherData))
handleGetWeather env (Header token) (Capture location) (Capture fromDay) (Capture interval) = Send $ do
  env.logger.info "get the weather forecast"
  whenAuth env token $ do
    mResult <- env.weather.get location fromDay interval
    pure $ case mResult of
      Just result -> ok result
      Nothing -> bad status400 "No data"

handleUpdateWeather ::
  Env ->
  Header "auth" AuthToken ->
  Body UpdateData ->
  Post (RespOr Text ())
handleUpdateWeather env (Header token) (Body updateData) = Send $ do
  env.logger.info "update the weather data"
  whenAuth env token $
    ok <$> env.weather.update updateData

whenAuth :: (ToJSON a) => Env -> AuthToken -> IO (RespOr Text a) -> IO (RespOr Text a)
whenAuth env token act = do
  isOk <- env.auth.validToken token
  if isOk
    then act
    else do
      logError env errMessage
      pure (bad status500 errMessage)
  where
    errMessage = "Token is invalid"

-- | server and handlers
module Server (
  server,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Data.Time
import Mig.Extra.Plugin.Trace qualified as Trace
import Mig.Json.IO

import Interface
import Server.Swagger
import Types

server :: Env -> Server IO
server env =
  setSwagger $
    withTrace $
      "api/v1/weather"
        /. [ auth
           , withAuth env $: app
           ]
  where
    auth = "get/auth-token" /. requestAuthToken env

    app =
      toServer
        [ "get/weather" /. getWeather env
        , "update" /. updateWeather env
        ]

    withTrace = applyPlugin (Trace.logHttpBy (logInfo env) Trace.V2)

-------------------------------------------------------------------------------------
-- application handlers

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

updateWeather ::
  Env ->
  Body UpdateData ->
  Post (RespOr Text ())
updateWeather env (Body updateData) = Send $ do
  logInfo @Text env "update the weather data"
  ok <$> env.weather.update updateData

-------------------------------------------------------------------------------------
-- authorization

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

withAuth :: Env -> Header "auth" AuthToken -> Plugin IO
withAuth env (Header token) = processResponse $ \getResp -> do
  isOk <- env.auth.validToken token
  if isOk
    then getResp
    else do
      logError env errMessage
      pure $ Just (bad status500 $ Text.encodeUtf8 errMessage)
  where
    errMessage = "Token is invalid"

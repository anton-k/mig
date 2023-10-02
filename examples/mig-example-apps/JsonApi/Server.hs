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
import Types

server :: Env -> Server IO
server env =
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

handleAuthToken :: Env -> Body User -> Post (RespOr Text AuthToken)
handleAuthToken env (Body user) = Send $ do
  env.logger.info ("get new auth token for: " <> user.name)
  isValid <- env.auth.validUser user
  if isValid
    then do
      token <- env.auth.newToken user
      void $ forkIO $ setExpireTimer token
      pure $ Right $ okResp token
    else do
      env.logger.error "User does not have access to service"
      pure $ Left $ badResp status500 "User is not valid"
  where
    setExpireTimer token = do
      threadDelay (1_000_000 * 60 * 10) -- 10 minutes
      env.auth.expireToken token

handleGetWeather ::
  Env ->
  Query "auth" AuthToken ->
  Capture "location" Location ->
  Capture "day" Day ->
  Capture "day-interval" DayInterval ->
  Get (RespOr Text (Timed WeatherData))
handleGetWeather env (Query token) (Capture location) (Capture fromDay) (Capture interval) = Send $ do
  env.logger.info "get the weather forecast"
  whenAuth env token $ do
    mResult <- env.weather.get location fromDay interval
    case mResult of
      Just result -> pure $ Right $ okResp result
      Nothing -> pure $ Left $ badResp status400 "No data"

handleUpdateWeather ::
  Env ->
  Query "auth" AuthToken ->
  Body UpdateData ->
  Post ()
handleUpdateWeather env (Query token) (Body updateData) = Send $ do
  env.logger.info "update the weather data"
  void $
    whenAuth env token $
      Right . okResp <$> env.weather.update updateData

whenAuth :: Env -> AuthToken -> IO (RespOr Text a) -> IO (RespOr Text a)
whenAuth env token act = do
  isOk <- env.auth.validToken token
  if isOk
    then act
    else do
      env.logger.error errMessage
      pure $ Left $ badResp status500 errMessage
  where
    errMessage = "Token is invalid"

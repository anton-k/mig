-- | server and handlers
module Server (
  server,
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Time
import Mig.Json.IO

import Interface
import Types

server :: Env -> Server IO
server env =
  "api"
    /. "v1"
    /. "weather"
    /. mconcat
      [ "get"
          /. mconcat
            [ "weather" /. handleGetWeather env
            , "auth-token" /. handleAuthToken env
            ]
      , "update" /. handleUpdateWeather env
      ]

handleAuthToken :: Env -> Body User -> Post (Either (Error Text) AuthToken)
handleAuthToken env (Body user) = Post $ do
  env.logger.info ("get new auth token for: " <> user.name)
  isValid <- env.auth.validUser user
  if isValid
    then do
      token <- env.auth.newToken user
      void $ forkIO $ setExpireTimer token
      pure $ Right token
    else do
      env.logger.error "User does not have access to service"
      pure $ Left $ Error status500 "User is not valid"
  where
    setExpireTimer token = do
      threadDelay (1_000_000 * 60 * 10) -- 10 minutes
      env.auth.expireToken token

handleGetWeather ::
  Env ->
  Query "auth" AuthToken ->
  Capture Location ->
  Capture Day ->
  Capture DayInterval ->
  Get (Either (Error Text) (Timed WeatherData))
handleGetWeather env (Query token) (Capture location) (Capture fromDay) (Capture interval) = Get $ do
  env.logger.info "get the weather forecast"
  fmap join $ whenAuth env token $ do
    mResult <- env.weather.get location fromDay interval
    case mResult of
      Just result -> pure $ Right result
      Nothing -> pure $ Left $ Error status400 "No data"

handleUpdateWeather ::
  Env ->
  Query "auth" AuthToken ->
  Body UpdateData ->
  Post ()
handleUpdateWeather env (Query token) (Body updateData) = Post $ do
  env.logger.info "update the weather data"
  void $
    whenAuth env token $
      env.weather.update updateData

whenAuth :: Env -> AuthToken -> IO a -> IO (Either (Error Text) a)
whenAuth env token act = do
  isOk <- env.auth.validToken token
  if isOk
    then Right <$> act
    else do
      env.logger.error errMessage
      pure $ Left $ Error status500 errMessage
  where
    errMessage = "Token is invalid"

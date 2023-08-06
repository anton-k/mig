-- 440784
{-# Language
      OverloadedStrings,
      OverloadedRecordDot,
      DeriveAnyClass,
      DerivingStrategies,
      DuplicateRecordFields,
      DataKinds,
      RecordWildCards
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Let's build a weather forecast API
--
-- We can query weather info. And also we can update the data.
-- User should get auth token that expires prior to making queries.
module Main
  ( main
  ) where

import Mig.Json.IO
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.IORef
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time
import Data.Time.Format.ISO8601
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import System.Random
import Data.List qualified as List

main :: IO ()
main = do
  env <- initEnv
  putStrLn ("The weather forecast JSON API server listens on port: " <> show port)
  runServer port (server env)
  where
    port = 8085

-------------------------------------------------------------------------------------
-- server and handlers

server :: Env -> Server IO
server env =
  "api" /. "v1" /. "weather" /.
    mconcat
      [ "get" /.
            mconcat
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
  Query "auth" AuthToken -> Capture Location -> Capture Day -> Capture DayInterval ->
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
  Query "auth" AuthToken -> Body UpdateData ->
  Post ()
handleUpdateWeather env (Query token) (Body updateData) = Post $ do
  env.logger.info "update the weather data"
  void $ whenAuth env token $
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

-------------------------------------------------------------------------------------
-- types

-- | Site's environment. It contains all interfaces
data Env = Env
  { weather :: Weather
  , auth :: Auth
  , logger :: Logger
  }

-- logger interface
data Logger = Logger
  { info :: Text -> IO ()
  , debug :: Text -> IO ()
  , error :: Text -> IO ()
  }

-- authorization interface
data Auth = Auth
  { newToken :: User -> IO AuthToken
  , validUser :: User -> IO Bool
  , validToken :: AuthToken -> IO Bool
  , expireToken :: AuthToken -> IO ()
  }

-- weather forecast interface
data Weather = Weather
  { get :: Location -> Day -> DayInterval -> IO (Maybe (Timed WeatherData))
  , update :: UpdateData -> IO ()
  }

-- domain types

-- auth domain

data User = User
  { name :: Text
  , pass :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype AuthToken = AuthToken Text
  deriving newtype (ToJSON, FromJSON, FromText, Eq, Ord, Show)

-- weather domain

newtype DayInterval = DayInterval Int
  deriving newtype (ToJSON, FromJSON, FromText)

data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

newtype Location = Location Text
  deriving newtype (ToJSON, FromJSON, FromText, Eq, Ord, Show)

data WeatherData = WeatherData
  { temperature :: Int
  , windSpeed :: Int
  , sunRainRatio :: Int
  , pressure :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

-- | Update weather data
data UpdateData = UpdateData
  { day :: Day
  , location :: Location
  , content :: WeatherData
  }
  deriving (Generic, ToJSON, FromJSON)

instance FromText Day where
  fromText = iso8601ParseM . Text.unpack

-------------------------------------------------------------------------------------
-- mutable state

-- | Mutable state of the server
--
-- IORefs here are only for simplicity. Use TVars in real servers
data St = St
  { users :: IORef (Map Text Text)
    -- ^ valid users with passwords
  , tokens :: IORef (Set AuthToken)
    -- ^ DB of auth tokens
  , weatherData :: IORef (Map Location (Map Day WeatherData))
    -- ^ waether DB
  }

-------------------------------------------------------------------------------------
-- interfaces

atomicModify :: IORef a -> (a -> a) -> IO ()
atomicModify ref f = atomicModifyIORef' ref (\st -> (f st, ()))

-- | allocate site interfaces
initEnv :: IO Env
initEnv = do
  st <- initSt
  pure $ Env
    { weather = initWeather st
    , auth = initAuth st
    , logger = initLogger st
    }

initAuth :: St -> Auth
initAuth st =
  Auth
    { newToken = \_user -> do
        token <- AuthToken . toText <$> randomRIO @Int (0, 1_000_000)
        atomicModify st.tokens $ Set.insert token
        pure token

    , validUser = \user -> ((== Just user.pass) . Map.lookup user.name) <$> readIORef st.users

    , validToken = \token -> Set.member token <$> readIORef st.tokens

    , expireToken = \token -> atomicModify st.tokens $ Set.delete token
    }

initWeather :: St -> Weather
initWeather st =
  Weather
    { get = \location day interval -> do
        locationMap <- readIORef st.weatherData
        fmap join $ forM (Map.lookup location locationMap) $ \weatherMap -> do
          let
            mWeatherByDays = mapM (flip Map.lookup weatherMap) $ toDaySpan day interval
          pure $ Timed day <$> mWeatherByDays

    , update = \(UpdateData day location content) -> atomicModify st.weatherData (Map.adjust (Map.insert day content) location)
    }

toDaySpan :: Day -> DayInterval -> [Day]
toDaySpan day (DayInterval count) = List.unfoldr go (day, count)
  where
    go (d, n)
      | n <= 0 = Nothing
      | otherwise = Just (succ d, (succ d, n - 1))

initLogger :: St -> Logger
initLogger _st =
  Logger
    { error = logBy "ERROR"
    , info = logBy "INFO"
    , debug = logBy "DEBUG"
    }
  where
    logBy level msg = Text.putStrLn $ mconcat ["[", level, "]: ", msg]

-------------------------------------------------------------------------------------
-- mock weather data, in real case we would connect to DB

-- | Init internal mutable state
initSt :: IO St
initSt = do
  now <- (.utctDay) <$> getCurrentTime
  St
    <$> initUsers
    <*> initTokens
    <*> initWeatherData now
  where
    initUsers = newIORef $ Map.fromList [("john", "123"), ("mary", "456")]

    initTokens = newIORef mempty

    initWeatherData now = do
      locationMap <- fmap Map.fromList $ mapM (\loc -> (loc, ) <$> getWeatherMap) locations
      newIORef locationMap
      where
        days = toDaySpan now (DayInterval 60)

        getWeatherMap = do
          baseTemperature <- randomRIO (0, 30)
          fmap Map.fromList $ mapM (\d -> (d, ) <$> genWeather baseTemperature) days

-- | test locations
locations :: [Location]
locations = Location <$> ["moscow", "berlin", "sochi", "amsterdam", "oslo", "maykop"]

-- | Generate random weather data
genWeather :: Int -> IO WeatherData
genWeather baseTemperature = do
  temperature <- (baseTemperature + ) <$> randomRIO (-5, 5)
  windSpeed <- randomRIO (0, 20)
  sunRainRatio <- randomRIO (0, 100)
  pressure <- randomRIO (720, 740)
  pure WeatherData{..}

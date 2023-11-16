-- Implements interfaces and state constants
module Init (
  initEnv,
) where

import Control.Monad
import Data.IORef
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Mig.Tool.Log.Fast qualified as Log
import System.Random

import Interface
import Internal.State
import Types

atomicModify :: IORef a -> (a -> a) -> IO ()
atomicModify ref f = atomicModifyIORef' ref (\st -> (f st, ()))

-- | allocate site interfaces
initEnv :: Int -> IO Env
initEnv port = do
  st <- initSt =<< initStateConfig
  proc <- initProc port
  pure $
    Env
      { weather = initWeather st
      , auth = initAuth st
      , proc = proc
      }

initProc :: Int -> IO Proc
initProc port = do
  logger <- Log.newLoggerStdout Log.LogYaml
  let Log.LogFuns{..} = Log.logFuns logger
  pure $
    Proc
      { logger = logger
      , startup = logInfo @Text $ ("App starts on port: " <> Text.pack (show port))
      , cleanup = do
          logInfo @Text "App shutdown"
          logger.close.run
      }

initAuth :: St -> Auth
initAuth st =
  Auth
    { newToken = \_user -> do
        token <- AuthToken . Text.pack . show <$> randomRIO @Int (0, 1_000_000)
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
          let mWeatherByDays = mapM (flip Map.lookup weatherMap) $ toDaySpan day interval
          pure $ Timed day <$> mWeatherByDays
    , update = \(UpdateData day location content) -> atomicModify st.weatherData (Map.adjust (Map.insert day content) location)
    }

toDaySpan :: Day -> DayInterval -> [Day]
toDaySpan day (DayInterval count) = List.unfoldr go (day, count)
  where
    go (d, n)
      | n <= 0 = Nothing
      | otherwise = Just (succ d, (succ d, n - 1))

initStateConfig :: IO InitSt
initStateConfig = do
  now <- (.utctDay) <$> getCurrentTime
  InitSt users <$> initWeatherData now
  where
    users = Map.fromList [("john", "123"), ("mary", "456")]

    initWeatherData now = do
      fmap Map.fromList $ mapM (\loc -> (loc,) <$> getWeatherMap) locations
      where
        days = toDaySpan now (DayInterval 60)

        getWeatherMap = do
          baseTemperature <- randomRIO (0, 30)
          fmap Map.fromList $ mapM (\d -> (d,) <$> genWeather baseTemperature) days

-- | test locations
locations :: [Location]
locations = Location <$> ["moscow", "berlin", "sochi", "amsterdam", "oslo", "maykop"]

-- | Generate random weather data
genWeather :: Int -> IO WeatherData
genWeather baseTemperature = do
  temperature <- (baseTemperature +) <$> randomRIO (-5, 5)
  windSpeed <- randomRIO (0, 20)
  sunRainRatio <- randomRIO (0, 100)
  pressure <- randomRIO (720, 740)
  pure WeatherData{..}

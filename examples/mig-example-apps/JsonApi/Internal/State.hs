-- | mutable state
module Internal.State (
  St (..),
  initSt,
) where

import Data.IORef
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Time
import System.Random
import Types

{-| Mutable state of the server

IORefs here are only for simplicity. Use TVars in real servers
-}
data St = St
  { users :: IORef (Map Text Text)
  -- ^ valid users with passwords
  , tokens :: IORef (Set AuthToken)
  -- ^ DB of auth tokens
  , weatherData :: IORef (Map Location (Map Day WeatherData))
  -- ^ waether DB
  }

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
      locationMap <- fmap Map.fromList $ mapM (\loc -> (loc,) <$> getWeatherMap) locations
      newIORef locationMap
      where
        days = toDaySpan now (DayInterval 60)

        getWeatherMap = do
          baseTemperature <- randomRIO (0, 30)
          fmap Map.fromList $ mapM (\d -> (d,) <$> genWeather baseTemperature) days

toDaySpan :: Day -> DayInterval -> [Day]
toDaySpan day (DayInterval count) = List.unfoldr go (day, count)
  where
    go (d, n)
      | n <= 0 = Nothing
      | otherwise = Just (succ d, (succ d, n - 1))

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

-- Implements interfaces
module Init
  ( initEnv
  ) where

import Control.Monad
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.IORef
import System.Random

import Interface
import Types
import Internal.State

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


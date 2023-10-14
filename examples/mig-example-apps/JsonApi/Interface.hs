module Interface (
  Env (..),
  LogFun,
  Proc (..),
  Logger (..),
  logInfo,
  logDebug,
  logError,
  Auth (..),
  Weather (..),
) where

import Data.Aeson (ToJSON (..), Value)
import Types

-- | Site's environment. It contains all interfaces
data Env = Env
  { weather :: Weather
  , auth :: Auth
  , proc :: Proc
  }

-- | server lifesycle interface
data Proc = Proc
  { startup :: IO ()
  , cleanup :: IO ()
  , logger :: Logger
  }

type LogFun = Value -> IO ()

logInfo :: (ToJSON a) => Env -> a -> IO ()
logInfo env = env.proc.logger.info . toJSON

logDebug :: (ToJSON a) => Env -> a -> IO ()
logDebug env = env.proc.logger.debug . toJSON

logError :: (ToJSON a) => Env -> a -> IO ()
logError env = env.proc.logger.error . toJSON

-- logger interface
data Logger = Logger
  { info :: LogFun
  , debug :: LogFun
  , error :: LogFun
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

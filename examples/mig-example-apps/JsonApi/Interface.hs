module Interface (
  Env (..),
  Proc (..),
  Auth (..),
  Weather (..),
  getLogger,
  module X,
) where

import Mig.Tool.Log as X (Log, LogFuns (..), LogNamespace, addLogNamespace, logFuns)
import Types

getLogger :: Env -> LogNamespace -> LogFuns
getLogger env ns = logFuns (addLogNamespace ns env.proc.logger)

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
  , logger :: Log
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

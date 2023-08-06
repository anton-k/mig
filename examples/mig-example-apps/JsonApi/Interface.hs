module Interface
  ( Env (..)
  , Logger (..)
  , Auth (..)
  , Weather (..)
  ) where

import Types

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

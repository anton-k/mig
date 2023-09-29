-- | mutable state
module Internal.State (
  St (..),
  initSt,
  InitSt (..),
) where

import Data.IORef
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time
import Types

{-| Mutable state of the server

IORefs here are only for simplicity. Use TVars in real servers
-}
data St = St
  { users :: IORef (Map UserName UserPass)
  -- ^ valid users with passwords
  , tokens :: IORef (Set AuthToken)
  -- ^ DB of auth tokens
  , weatherData :: IORef (Map Location (Map Day WeatherData))
  -- ^ waether DB
  }

type UserName = Text

type UserPass = Text

data InitSt = InitSt
  { users :: Map UserName UserPass
  , weather :: Map Location (Map Day WeatherData)
  }

-- | Init internal mutable state
initSt :: InitSt -> IO St
initSt config = do
  St
    <$> newIORef config.users
    <*> initTokens
    <*> newIORef config.weather
  where
    initTokens = newIORef mempty

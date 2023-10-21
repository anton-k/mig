{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | domain types
module Types (
  -- * Auth
  User (..),
  AuthToken (..),

  -- * Weather
  DayInterval (..),
  Timed (..),
  Location (..),
  WeatherData (..),
  UpdateData (..),
  module X,
) where

import Data.Time as X (Day)
import Mig.Json.IO

-- auth domain

data User = User
  { name :: Text
  , pass :: Text
  }

newtype AuthToken = AuthToken Text

-- weather domain

newtype DayInterval = DayInterval Int

data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

deriving instance (ToSchema a) => ToSchema (Timed a)

newtype Location = Location Text

data WeatherData = WeatherData
  { temperature :: Int
  , windSpeed :: Int
  , sunRainRatio :: Int
  , pressure :: Int
  }

-- | Update weather data
data UpdateData = UpdateData
  { day :: Day
  , location :: Location
  , content :: WeatherData
  }

-- derivings

mapDerive deriveNewtypeHttp [''AuthToken, ''DayInterval, ''Location]
mapDerive deriveBody [''User, ''WeatherData, ''UpdateData]

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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text as X (Text)
import Data.Time as X (Day)
import GHC.Generics
import Mig.Json.IO (FromHttpApiData (..))

-- auth domain

data User = User
  { name :: Text
  , pass :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype AuthToken = AuthToken Text
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show)

-- weather domain

newtype DayInterval = DayInterval Int
  deriving newtype (ToJSON, FromJSON, FromHttpApiData)

data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

newtype Location = Location Text
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show)

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

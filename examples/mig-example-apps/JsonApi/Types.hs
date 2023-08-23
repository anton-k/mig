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
import Mig.Json.IO (FromHttpApiData (..), Generic, ToParamSchema, ToSchema)

-- auth domain

data User = User
  { name :: Text
  , pass :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype AuthToken = AuthToken Text
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show, ToParamSchema, ToSchema)

-- weather domain

newtype DayInterval = DayInterval Int
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema)

data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

deriving instance (ToSchema a) => ToSchema (Timed a)

newtype Location = Location Text
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, Eq, Ord, Show, ToParamSchema, ToSchema)

data WeatherData = WeatherData
  { temperature :: Int
  , windSpeed :: Int
  , sunRainRatio :: Int
  , pressure :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Update weather data
data UpdateData = UpdateData
  { day :: Day
  , location :: Location
  , content :: WeatherData
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

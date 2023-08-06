{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | domain types
module Types
  ( -- * Auth
    User (..)
  , AuthToken(..)
    -- * Weather
  , DayInterval (..)
  , Timed (..)
  , Location (..)
  , WeatherData (..)
  , UpdateData (..)
  , module X
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Time as X (Day)
import Data.Text as X (Text)
import Data.Text qualified as Text
import Mig.Json.IO (FromText (..))
import GHC.Generics
import Data.Time.Format.ISO8601

-- auth domain

data User = User
  { name :: Text
  , pass :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype AuthToken = AuthToken Text
  deriving newtype (ToJSON, FromJSON, FromText, Eq, Ord, Show)

-- weather domain

newtype DayInterval = DayInterval Int
  deriving newtype (ToJSON, FromJSON, FromText)

data Timed a = Timed
  { from :: Day
  , content :: [a]
  }
  deriving (Generic, ToJSON, FromJSON)

newtype Location = Location Text
  deriving newtype (ToJSON, FromJSON, FromText, Eq, Ord, Show)

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

instance FromText Day where
  fromText = iso8601ParseM . Text.unpack



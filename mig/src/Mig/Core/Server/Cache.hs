-- | LRU cache to speedup fetching of the route handler
module Mig.Core.Server.Cache (
  CacheConfig (..),
  CacheKey (..),
  CacheValue (..),
  RouteCache (..),
  newRouteCache,
  withCache,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Cache.LRU.IO (AtomicLRU)
import Data.Cache.LRU.IO qualified as Lru
import Data.Text (Text)
import Mig.Core.Api (CaptureMap)
import Mig.Core.Class.Route (Route)
import Network.HTTP.Types.Method (Method)

data CacheConfig = CacheConfig
  { size :: Int
  , cacheFilter :: CacheKey -> Bool
  }

data CacheKey = CacheKey
  { inputType :: ByteString
  , outputType :: ByteString
  , method :: Method
  , path :: [Text]
  }
  deriving (Show, Eq, Ord)

data CacheValue m = CacheValue
  { captures :: CaptureMap
  , route :: Route m
  }

data RouteCache m = RouteCache
  { cacheFilter :: CacheKey -> Bool
  , cache :: AtomicLRU CacheKey (CacheValue m)
  }

newRouteCache :: CacheConfig -> IO (RouteCache m)
newRouteCache config =
  RouteCache config.cacheFilter <$> Lru.newAtomicLRU (Just (fromIntegral config.size))

withCache :: RouteCache m -> (CacheKey -> Maybe (CacheValue m)) -> CacheKey -> IO (Maybe (CacheValue m))
withCache (RouteCache cacheFilter cache) f key = do
  mCacheResult <- liftIO $ Lru.lookup key cache
  case mCacheResult of
    Just result -> pure (Just result)
    Nothing -> do
      case f key of
        Just result -> do
          when (cacheFilter key) $ Lru.insert key result cache
          pure (Just result)
        Nothing -> pure Nothing

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

-- | Cache config
data CacheConfig = CacheConfig
  { size :: Int
  -- ^ how many items are allowed in the cache
  , cacheFilter :: CacheKey -> Bool
  -- ^ which route to cache
  }

-- | Route key identidfies the single item for caching
data CacheKey = CacheKey
  { inputType :: ByteString
  -- ^ value of "Content-Type" header
  , outputType :: ByteString
  -- ^ value of "Accept" header
  , method :: Method
  -- ^ http method
  , path :: [Text]
  -- ^ path to route (includes inlined captures)
  }
  deriving (Show, Eq, Ord)

-- | Cache value
data CacheValue m = CacheValue
  { captures :: CaptureMap
  -- ^ extracted capture map from the path
  , route :: Route m
  -- ^ route handler
  }

-- | Route cache
data RouteCache m = RouteCache
  { cacheFilter :: CacheKey -> Bool
  -- ^ which route to cache (if True the route is cached)
  , cache :: AtomicLRU CacheKey (CacheValue m)
  -- ^ cache map
  }

-- | Allocates new cache
newRouteCache :: CacheConfig -> IO (RouteCache m)
newRouteCache config =
  RouteCache config.cacheFilter <$> Lru.newAtomicLRU (Just (fromIntegral config.size))

-- | Caches the function of route finder
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

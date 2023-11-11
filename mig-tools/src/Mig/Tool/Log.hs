-- | Logger interface
module Mig.Tool.Log (
  Log (..),
  LogLevel (..),
  LogNamespace (..),
  LogItem (..),
  addLogNamespace,
  setLogLevel,
  trackLogTime,

  -- * functions
  logInfo,
  logWarn,
  logError,
  logDebug,

  -- * augment interfaces with logger
  logQuery,
  logSet,
  logGet,
  logQuerySet,
  logQueryGet,
  logProc,
) where

import Control.Monad
import Data.Aeson qualified as Json
import Data.Aeson.Types qualified as Json
import Data.Text (Text)
import Data.Time
import Mig.Tool.Base

data LogLevel = LogDebug | LogInfo | LogWarn | LogError
  deriving (Show, Eq, Ord)

instance Json.ToJSON LogLevel where
  toJSON = \case
    LogDebug -> Json.toJSON @Text "debug"
    LogInfo -> Json.toJSON @Text "info"
    LogWarn -> Json.toJSON @Text "warn"
    LogError -> Json.toJSON @Text "error"

newtype LogNamespace = LogNamespace [Text]
  deriving newtype (Semigroup, Monoid, Show, Eq, Json.ToJSON)

data LogItem = LogItem
  { level :: LogLevel
  , time :: Maybe UTCTime
  , namespace :: LogNamespace
  , message :: Json.Value
  }
  deriving (Show, Eq)

instance Json.ToJSON LogItem where
  toJSON item =
    Json.object
      [ "log"
          Json..= ( Json.object $
                      maybe id addTime item.time $
                        [ "level" Json..= item.level
                        , "namespace" Json..= item.namespace
                        , "message" Json..= item.message
                        ]
                  )
      ]
    where
      addTime :: UTCTime -> [Json.Pair] -> [Json.Pair]
      addTime t fields = ("time" Json..= t) : fields

data Log = Log
  { log :: Set LogItem
  , close :: Proc
  }

addLogNamespace :: LogNamespace -> Log -> Log
addLogNamespace namespace = mapLogItem (addLogNamespaceItem namespace)

setLogLevel :: LogLevel -> Log -> Log
setLogLevel level = mapLogItem (setLogLevelItem level)

trackLogTime :: Log -> Log
trackLogTime = mapLogItemIO setLogTimeItem

silentLogLevel :: LogLevel -> Log -> Log
silentLogLevel noLevel logger = logger{log = filterSet ((/= noLevel) . (.level)) logger.log}

-------------------------------------------------------------------------------------
-- log functions

logByLevel :: (Json.ToJSON a) => LogLevel -> Log -> a -> IO ()
logByLevel level logger message = do
  now <- getCurrentTime
  logger.log.set $
    LogItem
      { level
      , time = Just now
      , namespace = LogNamespace []
      , message = Json.toJSON message
      }

logInfo :: (Json.ToJSON a) => Log -> a -> IO ()
logInfo = logByLevel LogInfo

logDebug :: (Json.ToJSON a) => Log -> a -> IO ()
logDebug = logByLevel LogDebug

logWarn :: (Json.ToJSON a) => Log -> a -> IO ()
logWarn = logByLevel LogWarn

logError :: (Json.ToJSON a) => Log -> a -> IO ()
logError = logByLevel LogError

-------------------------------------------------------------------------------------
-- add logger to interface

logSet :: (Json.ToJSON a) => Log -> Text -> Set a -> Set a
logSet env name (Set act) = Set $ \a -> do
  logInfo env $
    Json.object
      [ "call" Json..= name
      , "argument" Json..= a
      ]
  act a

logGet :: (Json.ToJSON a) => Log -> Text -> Get a -> Get a
logGet = undefined

logQuery :: (Json.ToJSON a, Json.ToJSON b) => Log -> Text -> Query a b -> Query a b
logQuery = undefined

logQuerySet :: (Json.ToJSON a) => Log -> Text -> Query a b -> Query a b
logQuerySet = undefined

logQueryGet :: (Json.ToJSON b) => Log -> Text -> Query a b -> Query a b
logQueryGet = undefined

logProc :: Log -> Text -> Proc -> Proc
logProc = undefined

-------------------------------------------------------------------------------------
-- item transformations

mapLogItem :: (LogItem -> LogItem) -> Log -> Log
mapLogItem f logger =
  logger{log = contramap f logger.log}

mapLogItemIO :: (LogItem -> IO LogItem) -> Log -> Log
mapLogItemIO f logger =
  logger{log = updateSet logger.log}
  where
    updateSet (Set s) = (Set (s <=< f))

addLogNamespaceItem :: LogNamespace -> LogItem -> LogItem
addLogNamespaceItem ns item = item{namespace = mappend ns item.namespace}

setLogLevelItem :: LogLevel -> LogItem -> LogItem
setLogLevelItem level item = item{level}

setLogTimeItem :: LogItem -> IO LogItem
setLogTimeItem item = case item.time of
  Nothing -> (\now -> item{time = Just now}) <$> getCurrentTime
  Just _ -> pure item

-- | Logger interface
module Mig.Tool.Log (
  Log (..),
  addLogNamespace,
  setLogLevel,
  LogLevel (..),
  LogNamespace (..),
  LogItem (..),

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

import Data.Aeson qualified as Json
import Data.Text (Text)
import Mig.Tool.Base

data LogLevel = LogDebug | LogInfo | LogWarn | LogError

newtype LogNamespace = LogNamespace [Text]
  deriving newtype (Semigroup, Monoid)

data LogItem = LogItem
  { level :: LogLevel
  , namespace :: LogNamespace
  , message :: Json.Value
  }

data Log = Log
  { log :: Set LogItem
  , close :: Proc
  }

addLogNamespace :: LogNamespace -> Log -> Log
addLogNamespace namespace = mapLogItem (addLogNamespaceItem namespace)

setLogLevel :: LogLevel -> Log -> Log
setLogLevel level = mapLogItem (setLogLevelItem level)

-------------------------------------------------------------------------------------
-- log functions

logByLevel :: (Json.ToJSON a) => LogLevel -> Log -> a -> IO ()
logByLevel level logger message =
  logger.log.set $
    LogItem
      { level
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

logSet :: (Json.ToJSON a) => Log -> Text -> Set a -> Set a
logSet env name (Set act) = Set $ \a -> do
  env.log.set $
    LogItem LogInfo mempty $
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

addLogNamespaceItem :: LogNamespace -> LogItem -> LogItem
addLogNamespaceItem ns item = item{namespace = mappend ns item.namespace}

setLogLevelItem :: LogLevel -> LogItem -> LogItem
setLogLevelItem level item = item{level}

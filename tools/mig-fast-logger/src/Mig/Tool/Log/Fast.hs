-- | Init fast-logger
module Mig.Tool.Log.Fast (
  newLogger,
  newLoggerStdout,
  module X,
) where

import Data.Aeson qualified as Json
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Yaml qualified as Yaml
import Mig.Tool.Base
import Mig.Tool.Log as X
import System.Log.FastLogger qualified as FastLogger

data LogCodec = LogJson | LogYaml

newLogger :: LogCodec -> FastLogger.LogType -> IO Log
newLogger codec config = do
  (writeLog, closeLogger) <- FastLogger.newFastLogger config
  pure $
    Log
      { log = Set (writeLog . toLogStr)
      , close = Proc closeLogger
      }
  where
    toLogStr :: LogItem -> FastLogger.LogStr
    toLogStr = FastLogger.toLogStr . toByteString

    toByteString :: LogItem -> ByteString
    toByteString = case codec of
      LogJson -> Json.encode
      LogYaml -> fromStrict . Yaml.encode

newLoggerStdout :: LogCodec -> IO Log
newLoggerStdout codec = newLogger codec (FastLogger.LogStdout FastLogger.defaultBufSize)

module Mig.Tool.Db.PostgreSQL.Simple (
  DbConnection (..),
  Db (..),
  newDbConnection,
  newDbConnectionPool,
  toDb,
  logDb,
  timedLogDb,
  module X,
) where

import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.Int as X (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Database.PostgreSQL.Simple as X (FromRow, In (..), Only (..), Query, ToRow)
import Database.PostgreSQL.Simple qualified as PostgreSQL
import Database.PostgreSQL.Simple.FromField as X (FromField (..))
import Database.PostgreSQL.Simple.ToField as X (ToField (..))
import Mig.Tool.Base hiding (Query)
import Mig.Tool.Log

data DbConnection = DbConnection
  { run :: forall a. (PostgreSQL.Connection -> IO a) -> IO a
  , close :: Proc
  }

data Db = Db
  { query :: forall q r. (ToRow q, FromRow r) => Query -> q -> IO [r]
  , query_ :: forall r. (FromRow r) => Query -> IO [r]
  , execute :: forall q. (ToRow q) => Query -> q -> IO Int64
  , execute_ :: Query -> IO Int64
  , executeMany :: forall q. (ToRow q) => Query -> [q] -> IO Int64
  , withTransaction :: forall a. IO a -> IO a
  , formatMany :: forall q. (ToRow q) => Query -> [q] -> IO ByteString
  , formatQuery :: forall q. (ToRow q) => Query -> q -> IO ByteString
  }

-- | Logs with SQL-query durations
timedLogDb :: Log -> Db -> Db
timedLogDb = undefined -- TODO

-- | Add logs to DB
logDb :: Log -> Db -> Db
logDb logger (Db query query_ execute execute_ executeMany withTransaction formatMany formatQuery) =
  Db
    { query = runQuery
    , query_ = runQuery_
    , execute = runExecute
    , execute_ = runExecute_
    , executeMany = runExecuteMany
    , withTransaction = runWithTransaction
    , formatMany
    , formatQuery
    }
  where
    LogFuns{..} = logFuns $ addLogNamespace "db" logger

    runQuery :: forall q r. (ToRow q, FromRow r) => Query -> q -> IO [r]
    runQuery expr q = do
      logDebug =<< (toQueryParamLogItem expr q)
      query expr q

    runQuery_ :: forall r. (FromRow r) => Query -> IO [r]
    runQuery_ expr = do
      logDebug =<< (toQueryParamLogItem expr ())
      query_ expr

    runExecute :: forall q. (ToRow q) => Query -> q -> IO Int64
    runExecute expr q = do
      logDebug =<< (toQueryParamLogItem expr q)
      execute expr q

    runExecute_ :: Query -> IO Int64
    runExecute_ expr = do
      logDebug =<< (toQueryParamLogItem expr ())
      execute_ expr

    runExecuteMany :: forall q. (ToRow q) => Query -> [q] -> IO Int64
    runExecuteMany expr q = do
      logDebug =<< (toManyLogItem expr q)
      executeMany expr q

    runWithTransaction :: forall a. IO a -> IO a
    runWithTransaction act = do
      logDebug $ sqlObject "begin transaction"
      res <- withTransaction act
      logDebug $ sqlObject "end transaction"
      pure res

    toQueryParamLogItem :: (ToRow q) => Query -> q -> IO Json.Value
    toQueryParamLogItem expr q = do
      eExpr <- Text.decodeUtf8' <$> formatQuery expr q
      pure $ case eExpr of
        Right res -> sqlObject res
        Left _err -> sqlObject "Failed to decode SQL expr to UTF-8 text"

    toManyLogItem :: (ToRow q) => Query -> [q] -> IO Json.Value
    toManyLogItem expr qs = do
      eExpr <- Text.decodeUtf8' <$> formatMany expr qs
      pure $ case eExpr of
        Right res -> sqlObject res
        Left _err -> sqlObject "Failed to decode SQL expr to UTF-8 text"

    sqlObject :: Text -> Json.Value
    sqlObject val = Json.object ["sql" Json..= val]

toDb :: DbConnection -> Db
toDb (DbConnection run _) =
  Db
    { query = runQuery
    , query_ = runQuery_
    , execute = runExecute
    , execute_ = runExecute_
    , executeMany = runExecuteMany
    , withTransaction = runWithTransaction
    , formatMany = runFormatMany
    , formatQuery = runFormatQuery
    }
  where
    runQuery :: forall q r. (ToRow q, FromRow r) => Query -> q -> IO [r]
    runQuery expr q = run (\conn -> PostgreSQL.query @q @r conn expr q)

    runQuery_ :: forall r. (FromRow r) => Query -> IO [r]
    runQuery_ expr = run (\conn -> PostgreSQL.query_ @r conn expr)

    runExecute :: forall q. (ToRow q) => Query -> q -> IO Int64
    runExecute expr q = run (\conn -> PostgreSQL.execute conn expr q)

    runExecute_ :: Query -> IO Int64
    runExecute_ expr = run (\conn -> PostgreSQL.execute_ conn expr)

    runExecuteMany :: forall q. (ToRow q) => Query -> [q] -> IO Int64
    runExecuteMany expr qs = run (\conn -> PostgreSQL.executeMany conn expr qs)

    runWithTransaction :: forall a. IO a -> IO a
    runWithTransaction act = run (\conn -> PostgreSQL.withTransaction conn act)

    runFormatMany :: forall q. (ToRow q) => Query -> [q] -> IO ByteString
    runFormatMany expr qs = run (\conn -> PostgreSQL.formatMany conn expr qs)

    runFormatQuery :: forall q. (ToRow q) => Query -> q -> IO ByteString
    runFormatQuery expr q = run (\conn -> PostgreSQL.formatQuery conn expr q)

newDbConnection :: ByteString -> IO DbConnection
newDbConnection connStr = do
  connection <- PostgreSQL.connectPostgreSQL connStr
  pure $
    DbConnection
      { run = \f -> f connection
      , close = Proc (PostgreSQL.close connection)
      }

newDbConnectionPool :: ByteString -> IO DbConnection
newDbConnectionPool = undefined

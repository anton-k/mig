module Mig.Tool.Db (
  Db (..),
  -- Sql (..),
  -- SqlResult (..),
  -- SqlRow,
  -- logDb,
) where

-- import Data.Aeson (ToJSON (..))
-- import Data.ByteString (ByteString)
-- import Data.Text qualified as Text
-- import Data.Text.Encoding qualified as Text
import Mig.Tool.Base

-- import Mig.Tool.Log

data Db conn = Db
  { run :: forall a. (conn -> IO a) -> IO a
  , close :: Proc
  }

{-
-- | Adds logging to all DB functions
logDb :: Log -> Db -> Db
logDb logger db =
  Db
    { query = logQuerySet logger "sqlQuery" db.query
    , close = logProc logger "dbClose" db.close
    }

newtype Sql = Sql ByteString

instance ToJSON Sql where
  toJSON (Sql expr) =
    case Text.decodeUtf8' expr of
      Right res -> toJSON res
      Left err -> toJSON (Text.unwords ["Failed to deocde SQL as text:", Text.pack (show err)])

newtype SqlResult = SqlResult
  { result :: Maybe [SqlRow]
  }

type SqlRow = [ByteString]
-}

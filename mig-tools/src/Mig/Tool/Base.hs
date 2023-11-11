-- | Basic interfaces
module Mig.Tool.Base (
  Query (..),
  queryToSet,
  queryToGet,
  QueryOr (..),
  Get (..),
  Set (..),
  filterSet,
  GetOr (..),
  Proc (..),
  module X,
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Contravariant as X

newtype Query a b = Query
  { query :: a -> IO b
  }

queryToSet :: Query a b -> Set a
queryToSet (Query f) = Set (void . f)

queryToGet :: a -> Query a b -> Get b
queryToGet val (Query f) = Get (f val)

newtype QueryOr a err b = QueryOr
  { query :: a -> IO (Either err b)
  }

newtype Set a = Set
  { set :: a -> IO ()
  }

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet f (Set g) = Set $ \a -> when (f a) (g a)

instance Contravariant Set where
  contramap f (Set a) = Set (a . f)

instance Semigroup (Set a) where
  (<>) (Set a) (Set b) = Set $ \x -> a x >> b x

instance Monoid (Set a) where
  mempty = Set $ const (pure ())

newtype Get a = Get
  { get :: IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

newtype GetOr err a = GetOr
  { get :: IO (Either err a)
  }
  deriving (Functor)

instance Applicative (GetOr err) where
  pure = GetOr . pure . pure
  (<*>) (GetOr fa) (GetOr fb) = GetOr (liftA2 (<*>) fa fb)

instance Monad (GetOr err) where
  (GetOr ma) >>= mf = GetOr $ do
    a <- ma
    case a of
      Right res -> do
        let GetOr mb = mf res
        mb
      Left err -> pure (Left err)

newtype Proc = Proc
  { run :: IO ()
  }

instance Semigroup Proc where
  (<>) (Proc a) (Proc b) = Proc (a >> b)

instance Monoid Proc where
  mempty = Proc (pure ())

-- | Module for HTML-based servers
module Mig.Json
  (
  -- * methods
    Get (..)
  , Post (..)
  , Put (..)
  , Delete (..)
  , Patch (..)
  , Options (..)

  -- * common
  -- | Common re-exports
  , module X
  ) where

import Mig.Common as X
import Mig (ToJsonResp (..))
import Mig.Internal.Types (toMethod)
import Network.HTTP.Types.Method

-- Get

newtype Get m a = Get (m a)

instance (Monad m, ToJsonResp a) => ToServer (Get m a) where
  type ServerMonad (Get m a) = m
  toServer (Get act) = toMethod methodGet (toJsonResp <$> act)

-- Post

newtype Post m a = Post (m a)

instance (Monad m, ToJsonResp a) => ToServer (Post m a) where
  type ServerMonad (Post m a) = m
  toServer (Post act) = toMethod methodPost (toJsonResp <$> act)

-- Put

newtype Put m a = Put (m a)

instance (Monad m, ToJsonResp a) => ToServer (Put m a) where
  type ServerMonad (Put m a) = m
  toServer (Put act) = toMethod methodPut (toJsonResp <$> act)

-- Delete

newtype Delete m a = Delete (m a)

instance (Monad m, ToJsonResp a) => ToServer (Delete m a) where
  type ServerMonad (Delete m a) = m
  toServer (Delete act) = toMethod methodDelete (toJsonResp <$> act)

-- Patch

newtype Patch m a = Patch (m a)

instance (Monad m, ToJsonResp a) => ToServer (Patch m a) where
  type ServerMonad (Patch m a) = m
  toServer (Patch act) = toMethod methodPatch (toJsonResp <$> act)

-- Options

newtype Options m a = Options (m a)

instance (Monad m, ToJsonResp a) => ToServer (Options m a) where
  type ServerMonad (Options m a) = m
  toServer (Options act) = toMethod methodOptions (toJsonResp <$> act)

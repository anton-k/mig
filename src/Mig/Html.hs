-- | Module for HTML-based servers
module Mig.Html
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
import Mig.Internal.Types (toMethod)
import Network.HTTP.Types.Method

-- Get

-- | Get method. Note that we can not use body input with Get-method, use Post for that.
-- So with Get we can use only URI inputs (Query, Optional, Capture)
newtype Get m a = Get (m a)

instance (Monad m, ToHtmlResp a) => ToServer (Get m a) where
  type ServerMonad (Get m a) = m
  toServer (Get act) = toMethod methodGet (toHtmlResp <$> act)

-- | Post method
newtype Post m a = Post (m a)

instance (Monad m, ToHtmlResp a) => ToServer (Post m a) where
  type ServerMonad (Post m a) = m
  toServer (Post act) = toMethod methodPost (toHtmlResp <$> act)

-- | Put method
newtype Put m a = Put (m a)

instance (Monad m, ToHtmlResp a) => ToServer (Put m a) where
  type ServerMonad (Put m a) = m
  toServer (Put act) = toMethod methodPut (toHtmlResp <$> act)

-- | Delete method
newtype Delete m a = Delete (m a)

instance (Monad m, ToHtmlResp a) => ToServer (Delete m a) where
  type ServerMonad (Delete m a) = m
  toServer (Delete act) = toMethod methodDelete (toHtmlResp <$> act)

-- | Patch method
newtype Patch m a = Patch (m a)

instance (Monad m, ToHtmlResp a) => ToServer (Patch m a) where
  type ServerMonad (Patch m a) = m
  toServer (Patch act) = toMethod methodPatch (toHtmlResp <$> act)

-- | Options method
newtype Options m a = Options (m a)

instance (Monad m, ToHtmlResp a) => ToServer (Options m a) where
  type ServerMonad (Options m a) = m
  toServer (Options act) = toMethod methodOptions (toHtmlResp <$> act)
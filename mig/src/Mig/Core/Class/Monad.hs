-- | Type-level function to extract underlying server monad
module Mig.Core.Class.Monad (
  MonadOf,
  Send (..),
) where

import Data.Kind
import Mig.Core.Types.Http
import Mig.Core.Types.Route

type family MonadOf a :: (Type -> Type) where
  MonadOf (Send method m a) = m
  MonadOf (Request -> m (Maybe Response)) = m
  MonadOf (f m) = m
  MonadOf (a -> b) = MonadOf b
  MonadOf [a] = MonadOf a

module Mig.Vm.Render
  ( renderServer
  ) where

import Mig.Vm.Types
import Mig.Vm.Class
import Control.Monad.State.Strict (runStateT)

renderServer ::
  forall a .
  (ToServer a, Monad (MonadOf a)) =>
  a -> MonadOf a (Ops, Ctx)
renderServer val = do
  let
    (Server api) = toServer val
  (ops, ctx) <- runStateT (apiToOps =<< sequenceA api) emptyCtx
  pure (ops, ctx)


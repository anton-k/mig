module Mig.Vm.Render
  ( renderServer
  ) where

import Mig.Vm.Types
import Control.Monad.State.Strict (runStateT)

renderServer :: Server m -> m (Ops, Ctx)
renderServer _server = runStateT go emptyCtx
  where
    go = undefined

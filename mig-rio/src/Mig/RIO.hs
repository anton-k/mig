{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Module provides instance of mig's class @HasServer@ for RIO type.
We can derive also @HasServer@ for any newtpye-wrapper on top of RIO-type.
It gives us ability to convert servers to IO-based ones and render them to warp servers.
-}
module Mig.RIO () where

import Mig (HasServer)
import RIO (RIO (..))

deriving newtype instance HasServer (RIO env)

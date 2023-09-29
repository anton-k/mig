{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mig.RIO () where

import Mig (HasServer)
import RIO (RIO (..))

deriving newtype instance HasServer (RIO env)

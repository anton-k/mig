module Mig.Vm.Class.Types 
  ( Send (..)
  , Query (..)
  , Header (..)
  , GET
  , POST 
  , PUT 
  ) where

import Data.Kind
import GHC.TypeLits

data GET
data POST
data PUT

newtype Query (sym :: Symbol) a = Query a 
newtype Header (sym :: Symbol) a = Header a 
newtype Send (method :: Type) m a = Send (m a)

module Test.Mig.Vm.Eval.Types where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Mig.Vm.Types 

data Req = Req
  { uri :: Path
  , method :: Method
  , headers :: [Header]
  , body :: Maybe Val
  , queries :: [QueryParam]
  }
  deriving (Show, Eq)

data Header = Header 
  { name :: !Text 
  , value :: !ByteString
  }
  deriving (Show, Eq)

data QueryParam = QueryParam 
  { name :: !Text
  , value :: !Text
  }
  deriving (Show, Eq)

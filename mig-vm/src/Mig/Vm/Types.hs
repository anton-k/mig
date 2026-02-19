module Mig.Vm.Types where

import Data.IORef
import Data.Text (Text)
import Data.ByteString (ByteString)

newtype Stack = Stack [Val]

data Val 
  = TVal Text
  | BVal ByteString
  | MVal Method

data Method = Get | Post | Put
  deriving (Eq)

newtype Ops = Ops [Op]

newtype StackRef = StackRef (IORef Stack)
newtype OpsRef = OpsRef (IORef Ops)

readStack :: StackRef -> IO Val
readStack = undefined

writeStack :: StackRef -> Val -> IO ()
writeStack = undefined

putOps :: OpsRef -> Ops -> IO ()
putOps = undefined

data Op 
  = GetUri 
  | GetUriPart Int
  | SaveCapture Text Text 
  | GetMethod
  | GetParam Text
  | GetCapture Text
  | GetBody 
  | GetHeader Text
  | SetHeader Text Text
  | SetBody ByteString
  | SetCode Int
  | SetError Text
  | SendResp
  | Fun Fun

type Fun = StackRef -> OpsRef -> IO ()

newtype Path = Path {unPath :: [PathItem]}
  deriving newtype (Show, Eq, Ord, Semigroup, Monoid)

-- | Path can be a static item or capture with a name
data PathItem
  = StaticPath Text
  | CapturePath Text
  deriving (Show, Eq, Ord)

instance Monoid (Api a) where
  mempty = Empty

instance Semigroup (Api a) where
  (<>) = Append

newtype Send method m a = Send (m a)

data Get
data Post
data Put

-- | HTTP API container
data Api a
  = -- | alternative between two API's
    Append (Api a) (Api a)
  | -- | an empty API that does nothing
    Empty
  | -- | path prefix for an API
    WithPath Path (Api a)
  | -- | handle route
    HandleRoute a
  deriving (Functor, Foldable, Traversable, Show, Eq)

type ServerFun m = m Ops

newtype Server m = Server (Api (ServerFun m))

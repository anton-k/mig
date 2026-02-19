module Mig.Vm.Types
  ( Val (..)
  , Op (..)
  , Ops (..)
  , Ctx (..)
  , FunIndex (..)
  , emptyCtx 
  , ctxInsertFun 
  , ctxIndex 
  , ctxGetFuns
  , Memory (..)
  , Fun
  , Method (..)
  , Api (..)
  , Path (..)
  , PathItem
  , pathToText 
  , Server (..)
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString (ByteString)
import Queue (Queue)
import Queue qualified as Queue

data Ctx = Ctx
  { funs :: Queue Fun
  , index :: !Int
  }

type Fun = Memory -> IO ()

emptyCtx :: Ctx
emptyCtx = Ctx
  { funs = Queue.empty
  , index = 0
  }

ctxIndex :: Ctx -> FunIndex
ctxIndex ctx = FunIndex ctx.index

ctxInsertFun :: Fun -> Ctx -> Ctx
ctxInsertFun f ctx = ctx
  { funs = Queue.enqueue f ctx.funs
  , index = ctx.index + 1
  }

ctxGetFuns :: Ctx -> [Fun]
ctxGetFuns ctx = Queue.toList ctx.funs

data Val 
  = TVal Text
  | BVal ByteString
  | MVal Method
  deriving (Show, Eq)

data Method = Get | Post | Put
  deriving (Show, Eq)

newtype Ops = Ops [Op]
  deriving (Show, Eq)

data Memory = Memory
  { readStack :: IO (Maybe Val)
  , writeStack :: Val -> IO ()
  , putOps :: Ops -> IO ()
  }

-- | Operators, all commands that VM supports
data Op 
  -- request
  = GetUri   
  | GetUriPart Int
  | SaveCapture Text
  | GetCapture Text
  | GetMethod
  | GetQuery Text
  | GetBody 
  | GetHeader Text
  | SetHeader Text ByteString
  | SetBody ByteString
  | SetCode Int
  -- handler
  | Fun FunIndex
  -- response
  | SendResp
  | SendText Text
  | SendByteString ByteString
  | SendError Text
  -- switch
  | WhenMethod Method Int
  | Case Val Int
  deriving (Show, Eq)

newtype FunIndex = FunIndex Int
  deriving (Show, Eq)

newtype Server m = Server (Api (ServerFun m))

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

newtype Path = Path {unPath :: [PathItem]}
  deriving stock (Show, Eq)

pathToText :: Path -> Text
pathToText (Path items) = Text.intercalate "/" items

type PathItem = Text

type ServerFun m = m Ops


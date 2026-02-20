module Mig.Vm.Types
  ( Val (..)
  , Resp (..)
  , Op (..)
  , CodeLabel (..)
  , Ops (..)
  , Ctx (..)
  , FunIndex (..)
  , emptyCtx 
  , ctxInsertFun 
  , ctxIndex 
  , ctxLabel 
  , ctxGetFuns
  , ctxBumpLabel
  , Memory (..)
  , Fun
  , Method (..)
  , Api (..)
  , Path (..)
  , PathItem
  , pathToText 
  , Server (..)
  , substCodeLabelsForCodeIndex 
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.ByteString (ByteString)
import Queue (Queue)
import Queue qualified as Queue
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap

data Ctx = Ctx
  { funs :: Queue Fun
  , index :: !Int
  , label :: !Int
  }

type Fun = Memory -> IO ()

emptyCtx :: Ctx
emptyCtx = Ctx
  { funs = Queue.empty
  , index = 0
  , label = 0
  }

ctxIndex :: Ctx -> FunIndex
ctxIndex ctx = FunIndex ctx.index

ctxLabel :: Ctx -> CodeLabel
ctxLabel ctx = CodeLabel ctx.label

ctxInsertFun :: Fun -> Ctx -> Ctx
ctxInsertFun f ctx = ctx
  { funs = Queue.enqueue f ctx.funs
  , index = ctx.index + 1
  }

ctxGetFuns :: Ctx -> [Fun]
ctxGetFuns ctx = Queue.toList ctx.funs

ctxBumpLabel :: Ctx -> Ctx
ctxBumpLabel ctx = ctx { label = ctx.label + 1 }

data Val 
  = TVal Text
  | BVal ByteString
  | MVal Method
  | RespVal Resp
  deriving (Show, Eq)

data Resp = Resp
  { status :: Int
  , headers :: [(ByteString, ByteString)]
  , body :: Maybe ByteString
  }
  deriving (Show, Eq)

data Method = Get | Post | Put
  deriving (Show, Eq)

newtype Ops = Ops [Op]
  deriving (Show, Eq)

data Memory = Memory
  { readStack :: IO (Maybe Val)
  , writeStack :: Val -> IO ()
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
  -- handler
  | Fun FunIndex
  -- response
  | SendResp
  -- switch
  | Goto CodeLabel 
  | Ifeq Val CodeLabel 
  | Label CodeLabel
  -- generic stack
  | Push Val
  | Pop
  | Dup
  deriving (Show, Eq)

newtype CodeLabel = CodeLabel Int
  deriving (Show, Eq)

substCodeLabelsForCodeIndex :: [Op] -> [Op]
substCodeLabelsForCodeIndex ops = 
  fmap substLabel ops
  where
    labelMap :: IntMap CodeLabel 
    labelMap = 
      foldl' accumLabel IntMap.empty (zip [0..] ops)
    
    accumLabel :: IntMap CodeLabel -> (Int, Op) -> IntMap CodeLabel
    accumLabel res = \case
      (index, Label (CodeLabel label)) -> IntMap.insert label (CodeLabel index) res
      _ -> res

  
    getCodeIndex :: CodeLabel -> CodeLabel
    getCodeIndex (CodeLabel x) = labelMap IntMap.! x

    substLabel :: Op -> Op
    substLabel = \case
      Goto label -> Goto (getCodeIndex label)
      Ifeq val label -> Ifeq val (getCodeIndex label)
      Label label -> Label (getCodeIndex label)
      x -> x 

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


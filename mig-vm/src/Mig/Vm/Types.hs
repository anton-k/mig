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
  , MediaType (..)
  , Api (..)
  , Path (..)
  , PathItem (..)
  , pathToText
  , Server (..)
  , substCodeLabelsForCodeIndex
  , apiToOps
  ) where

import Control.Monad (join)
import Queue (Queue)
import Queue qualified as Queue
import Mig.Vm.Types.Api
import Mig.Vm.Types.Cmd
import Control.Monad.State.Strict

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

freshLabel :: MonadState Ctx m => m CodeLabel
freshLabel = do
  ctx <- get
  put (ctxBumpLabel ctx)
  pure (CodeLabel ctx.label)

data Memory = Memory
  { readStack :: IO (Maybe Val)
  , writeStack :: Val -> IO ()
  }

newtype Server m = Server { unServer :: (Api (ServerFun m)) }
  deriving newtype (Semigroup, Monoid)

type ServerFun m = StateT Ctx m Ops

apiToOps :: forall m . MonadState Ctx m => Api Ops -> m Ops
apiToOps = renderApiIf . toApiIf
  where
    renderApiIf :: ApiIf Ops -> m Ops
    renderApiIf = fmap (Ops [GetUri] <> ) . \case
      EmptyApi -> pure emptyOps
      IfHandle method media th el -> ifHandle method media th =<< renderApiIf el
      IfPath path th el ->
        join $ liftA2 (ifPath path) (renderApiIf th) (renderApiIf el)

    emptyOps = Ops
      [ Push (RespVal $ Resp { status = 500, headers = [], body = Just "Not found" })
      , SendResp
      ]

    -- TODO: check media
    ifHandle method _media th el = do
      trueLabel <- freshLabel
      falseLabel <- freshLabel
      pure $ mconcat
        [ Ops
          [ MatchPath ""
          , Ifeq (BoolVal True) falseLabel
          , GetMethod
          , Ifeq (MVal method) falseLabel
          ]
        , th
        , Ops [ Goto trueLabel, Label falseLabel ]
        , el
        , Ops [ Label trueLabel ]
        ]

    ifPath path th el =
      ifOp [BoolVal True] (Ops [Dup, MatchPath (pathToText path)]) th (Ops [Pop] <> el)

ifOp :: MonadState Ctx m => [Val] -> Ops -> Ops -> Ops -> m Ops
ifOp vals cond th el = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  pure $ mconcat
    [ cond
    , Ops $ fmap (\val -> Ifeq val falseLabel) vals
    , th
    , Ops [ Goto trueLabel, Label falseLabel ]
    , el
    , Ops [ Label trueLabel ]
    ]


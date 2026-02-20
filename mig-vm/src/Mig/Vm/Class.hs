{-# Language UndecidableInstances #-}
module Mig.Vm.Class
  ( IsMethod (..)
  , IsOutput (..)
  , IsHandler
  , toRoute
  , module X
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Web.HttpApiData (FromHttpApiData (..))
import Data.Kind
import GHC.TypeLits
import Data.Text (Text)
import Data.Text qualified as Text
import Data.String (IsString (..))
import Data.Proxy
import Data.HList.HList
import Control.Monad (forM, join)
import Mig.Vm.Types
import Control.Monad.State.Strict (StateT (..))
import Mig.Vm.Class.Types as X
import Data.Text.Encoding qualified as Text

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))

class IsMethod a where
  toMethod :: Method

instance IsMethod GET where
  toMethod = Get

instance IsMethod POST where
  toMethod = Post

instance IsMethod PUT where
  toMethod = Put

class IsOutput a where
  toOutput :: a -> Resp

instance IsOutput Text where
  toOutput = textResp

instance IsOutput Int where
  toOutput val = textResp (Text.show val)

errorResp :: Int -> Text -> Resp
errorResp code txt = 
  (textResp txt) { status = code }

textResp :: Text -> Resp
textResp txt = Resp
    { status = 200
    , headers = [("Content-Type", "text/plain")]
    , body = Just (Text.encodeUtf8 txt)
    }  

class (IsMethod (MethodOf a), MonadUnliftIO (MonadOf a), IsOutput (ResOf a)) => IsHandler a where
  type MonadOf a :: Type -> Type 
  type MethodOf a :: Type
  type ArgOf a :: [Type]
  type ResOf a :: Type

  toHandler :: a -> Memory -> (HList (ArgOf a) -> MonadOf a (ResOf a))
  readArg :: Memory -> IO (Either Text (HList (ArgOf a)))
  toArgOps :: [Op]
  toArity :: Int

instance (IsMethod method, IsOutput a, MonadUnliftIO m) => IsHandler (Send method m a) where
  type MonadOf (Send method m a) = m
  type MethodOf (Send method m a) = method
  type ArgOf (Send method m a) = '[] 
  type ResOf (Send method m a) = a

  toHandler (Send getVal) _ = const getVal
  readArg = const (pure (Right HNil))
  toArgOps = []
  toArity = 0

instance (KnownSymbol sym, FromHttpApiData param, IsHandler a) => 
  IsHandler (Query sym param -> a) where
  type MonadOf (Query sym param -> a) = MonadOf a
  type MethodOf (Query sym param -> a) = MethodOf a
  type ArgOf (Query sym param -> a) = param ': ArgOf a
  type ResOf (Query sym param -> a) = ResOf a

  readArg memory = do
    eArgs <- readArg @a memory
    fmap join $ forM eArgs $ \args -> do
      eParam <- readQueryParam @param failedToParse memory
      pure $ 
        case eParam of
          Right param -> Right (HCons param args)
          Left msg -> Left (failedToParse <> ", " <> msg) 
    where
      failedToParse = "Failed to parse query: " <> getName @sym

  toHandler f memory arg = case arg of
    HCons param rest -> toHandler (f (Query param)) memory rest

  toArgOps = GetQuery (getName @sym) : toArgOps @a

  toArity = 1 + toArity @a

readQueryParam :: forall a. FromHttpApiData a => Text -> Memory -> IO (Either Text a)
readQueryParam errorMsg memory = do
  val <- memory.readStack
  pure $ case val of
    Just (TVal txt) -> 
      case parseQueryParam txt of
        Right param -> Right param
        Left msg -> Left (errorMsg  <> ", " <> msg) 
    _ -> Left errorMsg 

instance (KnownSymbol sym, FromHttpApiData param, IsHandler a) => 
  IsHandler (Header sym param -> a) where
  type MonadOf (Header sym param -> a) = MonadOf a
  type MethodOf (Header sym param -> a) = MethodOf a
  type ArgOf (Header sym param -> a) = param ': ArgOf a
  type ResOf (Header sym param -> a) = ResOf a

  readArg memory = do
    eArgs <- readArg @a memory
    fmap join $ forM eArgs $ \args -> do
      eParam <- readHeaderParam @param failedToParse memory
      pure $ 
        case eParam of
          Right param -> Right (HCons param args)
          Left msg -> Left (failedToParse <> ", " <> msg) 
    where
      failedToParse = "Failed to parse header: " <> getName @sym

  toHandler f memory arg = case arg of
    HCons param rest -> toHandler (f (Header param)) memory rest

  toArgOps = GetHeader (getName @sym) : toArgOps @a

  toArity = 1 + toArity @a


readHeaderParam :: forall a. FromHttpApiData a => 
  Text -> Memory -> IO (Either Text a)
readHeaderParam errorMsg memory = do
  val <- memory.readStack
  pure $ case val of
    Just (BVal bytes) -> 
      case parseHeader bytes of
        Right param -> Right param
        Left msg -> Left (errorMsg  <> ", " <> msg) 
    _ -> Left errorMsg 



toRoute :: forall a. IsHandler a => a -> StateT Ctx (MonadOf a) Ops
toRoute f = StateT $ \ctx -> withRunInIO $ \run -> 
  let 
    index = ctxIndex ctx
    label = ctxLabel ctx
  in 
    pure 
      ( Ops $ concat
          [ [ GetMethod
            , Ifeq (MVal $ toMethod @(MethodOf a)) label
            ]
          , toArgOps @a
          , [ Fun index
            , SendResp
            , Label label
            ]
          ]
      , ctxBumpLabel $ ctxInsertFun (run . handler) ctx
      )
  where
    handler :: Memory -> MonadOf a ()
    handler memory = do
      eArg <- liftIO (readArg @a memory)
      case eArg of
        Right arg -> liftIO . memory.writeStack . RespVal . toOutput @(ResOf a) =<< ((toHandler @a f) memory arg)
        Left errorMsg -> liftIO $ memory.writeStack (RespVal (errorResp 500 errorMsg))

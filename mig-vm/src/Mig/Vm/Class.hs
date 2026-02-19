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
  toOutput :: a -> Ops

instance IsOutput Text where
  toOutput val = Ops [SendText val]

instance IsOutput Int where
  toOutput val = Ops [SendText (Text.show val)]

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
  in 
    pure 
      ( Ops $ concat
          [ [ WhenMethod (toMethod @(MethodOf a)) (toArity @a + 1)]
          , toArgOps @a
          , [Fun index]
          ]
      , ctxInsertFun (run . handler) ctx
      )
  where
    handler :: Memory -> MonadOf a ()
    handler memory = do
      eArg <- liftIO (readArg @a memory)
      case eArg of
        Right arg -> liftIO . memory.putOps . toOutput @(ResOf a) =<< ((toHandler @a f) memory arg)
        Left errorMsg -> liftIO $ memory.putOps (Ops [SendError errorMsg])

{-
instance (IsMethod method, MonadUnliftIO m) => ToServer (Send method m Text) where
  type MonadOf (Send method m Text) = m

  toServer (Send readVal) = 
    withRunInIO $ \run -> 
      pure $ Ops
        [ GetMethod
        , whenMethod @method (run handler)
        ]
    where
      handler :: m Ops 
      handler = do
        val <- readVal
        pure $ Ops 
          [ SetBody (Text.encodeUtf8 val)
          , SetCode 200
          , SetHeader "Content-Type" "text/plain"
          , SendResp
          ]

instance (IsMethod method, MonadUnliftIO m) => ToServer (Send method m Int) where
  type MonadOf (Send method m Int) = m
  toServer (Send readVal) = 
    withRunInIO $ \run ->
      pure $ Ops
        [ GetMethod 
        , whenMethod @method (run handler)
        ]
    where
      handler :: m Ops
      handler = do
        val <- readVal
        pure $ Ops 
          [ SetBody (Text.encodeUtf8 $ Text.show val)
          , SetCode 200
          , SetHeader "Content-Type" "text/plain"
          , SendResp
          ]

whenMethod :: forall method . IsMethod method => IO Ops -> Op
whenMethod nextOps = Fun $ \stack ops -> do
  val <- readStack stack
  case val of
    MVal m | m == toMethod @method -> putOps ops =<< nextOps
    _ -> writeStack stack val


instance (KnownSymbol sym, FromHttpApiData param, ToServer a, MonadUnliftIO (MonadOf a)) => 
  ToServer (Query sym param -> a) where
  type MonadOf (Query sym param -> a) = MonadOf a

  toServer f = withRunInIO (\run -> pure $ Ops
    [ GetParam (getName @sym)
    , Fun (\stack ops -> run $ handler stack ops)
    ])
    where
      handler :: StackRef -> OpsRef -> (MonadOf a) ()
      handler stack ops = do
        arg <- liftIO $ readStack stack
        case arg of
          TVal txt -> 
            case parseQueryParam txt of
              Right p -> liftIO . putOps ops =<< toServer (f (Query p))
              Left msg -> liftIO $ putOps ops (Ops [SetError $ "Failed to parse param: " <> msg])
          _ -> liftIO $ writeStack stack arg


instance (KnownSymbol sym, FromHttpApiData header, ToServer a, MonadUnliftIO (MonadOf a)) => 
  ToServer (Header sym header -> a) where
  type MonadOf (Header sym header -> a) = MonadOf a

  toServer f = withRunInIO (\run -> pure $ Ops
    [ GetHeader (getName @sym)
    , Fun (\stack ops -> run $ handler stack ops)
    ])
    where
      handler :: StackRef -> OpsRef -> (MonadOf a) ()
      handler stack ops = do
        arg <- liftIO $ readStack stack
        case arg of
          BVal bs -> 
            case parseHeader bs of
              Right h -> liftIO . putOps ops =<< toServer (f (Header h))
              Left msg -> liftIO $ putOps ops (Ops [SetError $ "Failed to parse header: " <> msg])
          _ -> liftIO $ writeStack stack arg
-}

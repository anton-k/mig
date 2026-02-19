{-# Language UndecidableInstances #-}
module Mig.Vm.Class where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Web.HttpApiData (FromHttpApiData (..))
import Data.Kind
import GHC.TypeLits
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.String (IsString (..))
import Data.Proxy

import Mig.Vm.Types

class IsMethod a where
  toMethod :: Method

instance IsMethod Get where
  toMethod = Get

instance IsMethod Post where
  toMethod = Post

instance IsMethod Put where
  toMethod = Put

class MonadIO (MonadOf a) => ToServer a where
  type MonadOf a :: Type -> Type

  toServer :: a -> ServerFun (MonadOf a)

getName :: forall sym a. (KnownSymbol sym, IsString a) => a
getName = fromString (symbolVal (Proxy @sym))

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

newtype Query (sym :: Symbol) a = Query a 

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

newtype Header (sym :: Symbol) a = Header a 

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

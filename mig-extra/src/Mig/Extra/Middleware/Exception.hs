-- | Middlewares to handle exceptions
module Mig.Extra.Middleware.Exception (
  handleRespError,
) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.IO.Class

import Mig.Core

{-| Catches the run-time exceptions and converts them to responses.
This way of using errors is not recommended. Better use response of the type @RespOr@.
-}
handleRespError ::
  forall a b m.
  (MonadIO m, MonadCatch m, Exception a, IsResp b) =>
  (a -> m b) ->
  Middleware m
handleRespError handle = fromMiddlewareFun $ \f -> \req -> do
  eResult <- try @m @a (f req)
  case eResult of
    Right res -> pure res
    Left err -> Just . toResponse <$> handle err

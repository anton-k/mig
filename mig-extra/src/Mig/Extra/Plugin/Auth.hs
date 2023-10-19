-- | Plugin to handle authorization. Just a sketch for now.
module Mig.Extra.Plugin.Auth (
  WithAuth (..),
  withHeaderAuth,
) where

import Control.Monad.IO.Class
import Mig.Core

data WithAuth m token resp = WithAuth
  { isValid :: token -> m Bool
  -- ^ check that token is valid and return info
  , authFail :: token -> m resp
  }

withHeaderAuth :: forall m token resp. (IsResp resp, MonadIO m) => WithAuth m token resp -> Header "auth" token -> Plugin m
withHeaderAuth env (Header token) = processResponse $ \getResp -> do
  isOk <- env.isValid token
  if isOk
    then getResp
    else Just . toResponse <$> env.authFail token

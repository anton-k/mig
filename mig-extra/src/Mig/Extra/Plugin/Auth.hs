-- | Plugin to handle authorization. Just a sketch for now.
module Mig.Extra.Plugin.Auth (
  WithAuth (..),
  withHeaderAuth,
) where

import Control.Monad.IO.Class
import Mig.Core

-- | Authorization plugin interface
data WithAuth m token resp = WithAuth
  { isValid :: token -> m Bool
  -- ^ check that token is valid
  , authFail :: token -> m resp
  -- ^ how to respond on failure
  }

-- | Creates plugin that applies check of auth credentials which are passed as HTTP Header with name auth.
withHeaderAuth :: forall m token resp. (IsResp resp, MonadIO m) => WithAuth m token resp -> Header "auth" token -> Plugin m
withHeaderAuth env (Header token) = processResponse $ \getResp -> do
  isOk <- env.isValid token
  if isOk
    then getResp
    else Just . toResponse <$> env.authFail token

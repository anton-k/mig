{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Html servers
module Mig.Extra.Server.Html (
  -- * Http verbs
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
  Head,
  Trace,

  -- * Response
  Resp (..),
  RespOr,

  -- * utils
  Link (..),

  -- * re-exports
  Body (..),
  module X,
) where

import Mig.Core (Body (..))
import Mig.Core qualified as Core
import Mig.Extra.Server.Common as X
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as HA

-- response

newtype Resp a = Resp (Core.Resp Html a)
  deriving newtype (IsResp, Eq, Show, Functor)

type RespOr err a = Either (Resp err) (Resp a)

type Get m a = Send GET m (Resp a)
type Post m a = Send POST m (Resp a)
type Put m a = Send PUT m (Resp a)
type Delete m a = Send DELETE m (Resp a)
type Patch m a = Send PATCH m (Resp a)
type Options m a = Send OPTIONS m (Resp a)
type Head m a = Send HEAD m (Resp a)
type Trace m a = Send TRACE m (Resp a)

{-| HTML a-links, this type is useful for using
with template engines that rely on @ToJSON@ instance.
Also it can be rendered as Html with @ToMarkup@ instance.
-}
data Link = Link
  { href :: Url
  , name :: Text
  }
  deriving (Generic, ToJSON)

instance ToMarkup Link where
  toMarkup link = H.a H.! HA.href (renderUrl link.href) $ H.text link.name

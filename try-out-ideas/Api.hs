-- | With it we can restore some structure and possibly apply
-- different interpreters to build servers, clients, openapi schemas
--
-- only at the moment there is no info on the Json structure
module Mig.Internal.Api
  ( Api (..)
  , Server (..)
  , toApplication
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Aeson qualified as Json
import Data.Text (Text)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Header (HeaderName)
import Control.Monad
import Data.Maybe

import Mig.Internal.Types qualified as Mig
import Network.Wai
import Control.Monad.IO.Class

data Api a
  = AnyMethod a
  | ApiMethod Method a
  | WithBody (Json.Value -> Api a)
  | WithRawBody (BL.ByteString -> Api a)
  | WithPath [Text] (Api a)
  | WithCapture (Text -> Api a)
  | WithQuery ByteString (Maybe ByteString -> Api a)
  | WithHeader HeaderName (Maybe ByteString -> Api a)
  | Append (Api a) (Api a)
  | Empty
  deriving (Functor)

instance Monoid (Api a) where
  mempty = Empty

instance Semigroup (Api a) where
  (<>) = Append

-------------------------------------------------------------------------------------
-- server

newtype Server m = Server (Api (m (Maybe Mig.Resp)))
  deriving newtype (Semigroup, Monoid)

toApplication :: forall m . Server IO -> Application
toApplication (Server server) = undefined

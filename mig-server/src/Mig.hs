{-| Main module to write servers

Server is a function from response (Resp) to request (Req). Request is wrapped into monad.
Library supports IO-monad and ReaderT over IO like monads.

We can build servers from parts with flexible combinators.
Let's build hello-world server:

> main :: IO ()
> main = runServer 8080 server
>
> server :: Server IO
> server =
>   "api" /. "v1" /. "hello" /. handleHello
>
> handleHello :: Get Text IO Text
> handleHello = Send $ pure "Hello World"

We can iuse monoids to combine servers and newtype-wrappers to read various inputs.
See readme of the repo for tutorial and docs.
-}
module Mig (
  -- * types
  Server,
  Api,
  Path (..),
  PathItem (..),

  -- * DSL
  Json,
  ToServer (..),
  ToRoute (..),
  ToRouteInfo (..),

  -- ** methods
  Send (..),
  Get,
  Post,
  Put,
  Delete,
  Patch,
  Options,
  Head,
  Trace,
  IsMethod (..),
  GetMethod,
  PostMethod,
  PutMethod,
  DeleteMethod,
  PatchMethod,
  OptionsMethod,
  HeadMethod,
  TraceMethod,

  -- ** path and query

  -- | Build API for routes with queries and captures.
  -- Use monoid to combine several routes together.
  (/.),
  Capture (..),
  Query (..),
  Optional (..),
  Body (..),
  RawBody (..),
  Header (..),
  FormBody (..),
  PathInfo (..),

  -- ** response

  -- | How to modify response and attach specific info to it
  AddHeaders (..),
  SetStatus (..),
  setStatus,
  addHeaders,

  -- ** specific cases
  staticFiles,

  -- ** Errors

  -- | How to report errors
  Error (..),

  -- ** Low-level types
  Req,
  Resp,
  ServerFun (..),
  handleRespError,

  -- * Run

  -- | Run server application
  runServer,
  ServerConfig (..),
  toApplication,

  -- ** Render

  -- | Render Reader-IO monad servers to IO servers.
  HasServer (..),
  fromReader,

  -- * Convertes
  ToTextResp (..),
  ToJsonResp (..),
  ToHtmlResp (..),
  ToByteStringResp (..),
  ToText (..),

  -- * utils
  badRequest,
  prependServerAction,

  -- ** Server
  mapRouteInfo,
  mapServerFun,
  mapResp,

  -- ** OpenApi
  toOpenApi,
  setDescription,
  setSummary,
  module X,
) where

-- common codecs and types
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.OpenApi as X (OpenApi, ToParamSchema (..), ToSchema (..))
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Network.HTTP.Types.Header as X (RequestHeaders, ResponseHeaders)
import Network.HTTP.Types.Status as X
import Text.Blaze.Html as X (Html, ToMarkup (..))
import Web.FormUrlEncoded as X
import Web.HttpApiData as X

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, try)
import Mig.Core.Api (PathItem (..))
import Mig.Core.OpenApi (toOpenApi)
import Mig.Core.Server
import Mig.Core.Server.Class
import Mig.Core.Types (
  Error (..),
  Req,
  Resp,
  ToByteStringResp (..),
  ToHtmlResp (..),
  ToJsonResp (..),
  ToText (..),
  ToTextResp (..),
  addRespHeaders,
  badRequest,
  setRespStatus,
 )
import Mig.Server
import Mig.Server.Class

-- | Adds headers for response of the server
addHeaders :: (Monad m) => ResponseHeaders -> Server m -> Server m
addHeaders headers = mapResp $ addRespHeaders headers

-- | Sets status for response of the server
setStatus :: (Monad m) => Status -> Server m -> Server m
setStatus st = mapResp $ setRespStatus st

-- | Prepends action to the server
prependServerAction :: (Monad m) => Server m -> m () -> Server m
prependServerAction srv act = flip mapServerFun srv $ \(ServerFun f) -> ServerFun $ \req -> do
  act
  f req

handleRespError ::
  forall a m.
  (MonadCatch m, Exception a) =>
  (a -> m (Maybe Resp)) ->
  Server m ->
  Server m
handleRespError handle = mapServerFun $ \(ServerFun f) -> ServerFun $ \req -> do
  eResult <- try @m @a (f req)
  case eResult of
    Right res -> pure res
    Left err -> handle err

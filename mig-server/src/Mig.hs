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
  Server (..),
  Api (..),
  Path (..),
  PathItem (..),
  Route (..),

  -- * DSL
  Json,
  FormUrlEncoded,
  OctetStream,
  ToServer (..),
  ToRoute (..),
  ToRouteInfo (..),
  MediaType,
  ToMediaType (..),
  MimeRender (..),
  MimeUnrender (..),

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
  GET,
  POST,
  PUT,
  DELETE,
  PATCH,
  OPTIONS,
  HEAD,
  TRACE,

  -- ** path and query

  -- | Build API for routes with queries and captures.
  -- Use monoid to combine several routes together.
  (/.),
  Capture (..),
  Query (..),
  QueryFlag (..),
  Optional (..),
  ReqBody (..),
  Header (..),
  PathInfo (..),

  -- ** response

  -- | How to modify response and attach specific info to it
  Resp (..),
  okResp,
  badResp,
  setStatus,
  addHeaders,
  EitherResp,

  -- ** specific cases
  staticFiles,

  -- ** Low-level types
  Request,
  Response,
  mapServerFun,
  ServerFun,
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
  ToText (..),

  -- * utils
  badRequest,
  prependServerAction,

  -- ** Server
  mapRouteInfo,
  mapResponse,

  -- ** OpenApi
  toOpenApi,
  setDescription,
  describeInputs,
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
import Mig.Core.OpenApi (toOpenApi)
import Mig.Core.Server
import Mig.Core.Server.Class
import Mig.Core.Types (
  Request,
  Response,
  ToText (..),
  badRequest,
 )
import Mig.Core.Types.MediaType
import Mig.Core.Types.Response (
  Resp (..),
  addHeaders,
  badResp,
  okResp,
  setStatus,
 )
import Mig.Server.Class
import Mig.Server.Wai

-- | Prepends action to the server
prependServerAction :: (Monad m) => ServerFun m -> m () -> ServerFun m
prependServerAction f act = \req -> do
  act
  f req

handleRespError ::
  forall a m.
  (MonadCatch m, Exception a) =>
  (a -> m (Maybe Response)) ->
  Server m ->
  Server m
handleRespError handle = mapServerFun $ \f -> \req -> do
  eResult <- try @m @a (f req)
  case eResult of
    Right res -> pure res
    Left err -> handle err

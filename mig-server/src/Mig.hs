{-| Main module to write servers

Server is a function from response to request. Request is wrapped into monad.
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
> handleHello :: Get IO (Resp Text Text)
> handleHello = Send $ pure $ ok "Hello World"

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
  AnyMedia,
  FormUrlEncoded,
  OctetStream,
  ToServer (..),
  ToRoute (..),
  MediaType,
  ToMediaType (..),
  ToRespBody (..),
  FromReqBody (..),

  -- ** response
  IsResp (..),
  badReq,
  internalServerError,
  notImplemented,
  redirect,
  setHeader,
  setCookie,
  SetCookie (..),
  defCookie,

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

  -- ** safe URLs
  UrlOf,
  ToUrl (..),
  Url (..),
  renderUrl,
  (:|) (..),

  -- ** path and query

  -- | Build API for routes with queries and captures.
  -- Use monoid to combine several routes together.
  (/.),
  Capture (..),
  Query (..),
  QueryFlag (..),
  Optional (..),
  Body (..),
  OptionalHeader (..),
  Cookie (..),
  Header (..),
  PathInfo (..),
  FullPathInfo (..),
  RawRequest (..),

  -- ** response

  -- | How to modify response and attach specific info to it
  Resp (..),
  RespOr (..),

  -- ** specific cases
  staticFiles,

  -- ** Plugins
  Plugin (..),
  PluginFun,
  ToPlugin (..),
  applyPlugin,
  ($:),
  prependServerAction,
  appendServerAction,
  processResponse,

  -- ** Low-level types
  Request,
  Response,
  okResponse,
  badResponse,
  ServerFun,
  -- | Run server application
  runServer,
  runServer',
  ServerConfig (..),
  FindRouteType (..),
  CacheConfig (..),
  toApplication,

  -- ** Render

  -- | Render Reader-IO monad servers to IO servers.
  HasServer (..),
  fromReader,

  -- * Convertes
  ToText (..),

  -- * utils
  badRequest,

  -- ** Server
  mapRouteInfo,
  mapServerFun,
  mapResponse,
  atPath,
  filterPath,
  getServerPaths,
  addPathLink,

  -- ** OpenApi
  toOpenApi,
  setDescription,
  describeInputs,
  setSummary,
  module X,

  -- ** Swagger
  withSwagger,
  swagger,
  DefaultInfo (..),
  addDefaultInfo,
  writeOpenApi,
  printOpenApi,
) where

-- common codecs and types

import Control.Monad.IO.Class as X
import Control.Monad.Trans.Class as X
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.Default as X
import Data.Maybe as X
import Data.OpenApi as X (OpenApi, ToParamSchema (..), ToSchema (..))
import Data.String as X
import Data.Text as X (Text)
import GHC.Generics as X (Generic)
import Network.HTTP.Types.Header as X (RequestHeaders, ResponseHeaders)
import Network.HTTP.Types.Status as X
import Text.Blaze.Html as X (Html, ToMarkup (..))
import Web.FormUrlEncoded as X
import Web.HttpApiData as X

import Mig.Core
import Mig.Extra.Derive as X
import Mig.Server.Wai
import Mig.Server.Warp
import Mig.Swagger

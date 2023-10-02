-- | Module for common re-exports
module Mig.Server.Common (
  -- * types
  Server (..),
  Api (..),
  Path (..),
  PathItem (..),
  Route (..),

  -- * DSL
  Json,
  OctetStream,
  FormUrlEncoded,
  ToServer (..),
  ToRoute (..),
  ToRouteInfo (..),
  MediaType,
  ToMediaType (..),
  MimeRender (..),
  IsResp (..),

  -- ** methods
  Send (..),
  IsMethod (..),
  RespOr,
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

  -- ** specific cases
  staticFiles,

  -- ** Middlewares
  Middleware,
  ToMiddleware (..),
  applyMiddleware,
  prependServerAction,
  appendServerAction,
  processResponse,

  -- ** Low-level types
  Request,
  Response,
  okResponse,
  badResponse,
  badRequest,
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

  -- ** Server
  mapRouteInfo,
  mapServerFun,
  mapResponse,
  atPath,
  addPathLink,

  -- ** OpenApi
  toOpenApi,
  setDescription,
  describeInputs,
  setSummary,
  module X,
) where

import Mig hiding (
  Delete,
  Get,
  Head,
  Options,
  Patch,
  Post,
  Put,
  Trace,
 )

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

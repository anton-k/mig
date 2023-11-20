-- | Module for common re-exports
module Mig.Extra.Server.Common (
  -- * types
  Server (..),
  Api (..),
  Path (..),
  PathItem (..),
  Route (..),

  -- * DSL
  Json,
  AnyMedia,
  OctetStream,
  FormUrlEncoded,
  ToServer (..),
  ToRoute (..),
  MediaType,
  ToMediaType (..),
  ToRespBody (..),

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
  Header (..),
  OptionalHeader (..),
  Cookie (..),
  PathInfo (..),
  FullPathInfo (..),
  RawRequest (..),

  -- ** response

  -- | How to modify response and attach specific info to it

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
  badRequest,
  ServerFun,

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
  filterPath,
  getServerPaths,
  addPathLink,

  -- ** OpenApi
  toOpenApi,
  setDescription,
  describeInputs,
  setSummary,
  module X,
) where

import Mig.Core hiding (
  Delete,
  Get,
  Head,
  Options,
  Patch,
  Post,
  Put,
  Resp,
  RespOr,
  Trace,
 )

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
import Mig.Extra.Derive as X
import Network.HTTP.Types.Header as X (RequestHeaders, ResponseHeaders)
import Network.HTTP.Types.Status as X
import Text.Blaze.Html as X (Html, ToMarkup (..))
import Web.FormUrlEncoded as X
import Web.HttpApiData as X

-- | Module for HTML-based servers
module Mig.Common (
  -- * types
  Server (..),

  -- * DSL
  ToText (..),
  ToHtmlResp (..),
  FromText (..),
  ToServer (..),
  withServerAction,

  -- * path and query
  (/.),
  Capture (..),
  Query (..),
  Optional (..),
  Body (..),
  RawBody (..),
  Header (..),
  FormBody (..),
  PathInfo (..),

  -- * response
  AddHeaders (..),
  SetStatus (..),
  setStatus,
  addHeaders,

  -- * Errors
  Error (..),
  handleError,

  -- * Render
  HasServer (..),
  fromReader,

  -- * Run
  ServerConfig (..),
  toApplication,
  runServer,

  -- * utils
  badRequest,
  module X,
) where

import Mig (
  AddHeaders (..),
  Body (..),
  Capture (..),
  Error (..),
  FormBody (..),
  FromText (..),
  HasServer (..),
  Header (..),
  Optional (..),
  PathInfo (..),
  Query (..),
  RawBody (..),
  Server (..),
  ServerConfig (..),
  SetStatus (..),
  ToHtmlResp (..),
  ToServer (..),
  ToText (..),
  addHeaders,
  badRequest,
  fromReader,
  handleError,
  runServer,
  setStatus,
  toApplication,
  withServerAction,
  (/.),
 )

import Network.HTTP.Types.Status as X
import Web.FormUrlEncoded as X
import Web.HttpApiData as X

-- | Module for common re-exports
module Mig.Server.Common () where

{-
( -- * types
    Server (..)
  -- * DSL
  , ToText (..)
  , ToHtmlResp (..)
  , FromText (..)
  , ToServer (..)
  , withServerAction
  -- * path and query
  , (/.)
  , Capture (..)
  , Query (..)
  , Optional (..)
  , Body (..)
  , RawBody (..)
  , Header (..)
  , FormBody (..)
  , PathInfo (..)

  -- * response
  , AddHeaders (..)
  , SetStatus (..)
  , setStatus
  , addHeaders

  -- * Errors
  , Error (..)
  , handleError

  -- * Render
  , HasServer (..)
  , fromReader
  -- * Run
  , ServerConfig (..)
  , toApplication
  , runServer
  -- * utils
  , badRequest

  , module X
  ) where

import Mig
  ( Server (..), ToServer (..), ToText (..), ToHtmlResp (..), FromText (..), handleError, PathInfo (..)
  , (/.), Capture (..), Query (..), Optional (..), Body (..), RawBody (..), Header (..), FormBody (..), AddHeaders (..), SetStatus (..)
  , setStatus, addHeaders, HasServer (..), fromReader, ServerConfig (..), toApplication, runServer, badRequest, Error (..), withServerAction)

import Network.HTTP.Types.Status as X
import Web.HttpApiData as X
import Web.FormUrlEncoded as X
-}

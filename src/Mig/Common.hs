-- | Module for HTML-based servers
module Mig.Common
  ( -- * types
    Server (..)
  -- * DSL
  , ToLazyText (..)
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
  , RawFormData (..)
  , FormBody (..)
  , FormJson (..)
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
  , Config (..)
  , toApplication
  , runServer
  -- * utils
  , badRequest

  , module X
  ) where

import Mig
  ( Server (..), ToServer (..), ToLazyText (..), ToHtmlResp (..), FromText (..), handleError, PathInfo (..)
  , (/.), Capture (..), Query (..), Optional (..), Body (..), RawBody (..), Header (..), RawFormData (..), FormBody (..), FormJson (..), AddHeaders (..), SetStatus (..)
  , setStatus, addHeaders, HasServer (..), fromReader, Config (..), toApplication, runServer, badRequest, Error (..), withServerAction)

import Network.HTTP.Types.Status as X

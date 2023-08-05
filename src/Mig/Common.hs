-- | Module for HTML-based servers
module Mig.Common
  ( -- * types
    Server (..)
  -- * DSL
  , ToLazyText (..)
  , ToHtmlResp (..)
  , FromText (..)
  -- * path and query
  , (/.)
  , Capture (..)
  , Query (..)
  , Optional (..)
  , Body (..)
  , RawBody (..)
  , Header (..)

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
  ( Server (..), ToLazyText (..), ToHtmlResp (..), FromText (..), handleError
  , (/.), Capture (..), Query (..), Optional (..), Body (..), RawBody (..), Header (..), AddHeaders (..), SetStatus (..)
  , setStatus, addHeaders, HasServer (..), fromReader, Config (..), toApplication, runServer, badRequest, Error (..))

import Network.HTTP.Types.Status as X

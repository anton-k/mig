{-# LANGUAGE TemplateHaskell #-}

-- server
module Server (
  initServer,
) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import FileEmbedLzma

import Mig.Html.IO
import System.Random

import Api
import Interface
import Types
import View ()

-- server definition

initServer :: Site -> Server IO
initServer site = logRoutes $ server (initRoutes site) <> staticServer
  where
    staticServer :: Server IO
    staticServer =
      addFavicon $ "static" /. staticFiles resourceFiles

    resourceFiles :: [(FilePath, ByteString)]
    resourceFiles = $(embedRecursiveDir "Html/resources")

    addFavicon :: Server IO -> Server IO
    addFavicon = addPathLink "favicon.ico" "static/lambda-logo.png"

    logRoutes :: Server IO -> Server IO
    logRoutes = applyPlugin $ \(FullPathInfo path) -> prependServerAction $
      when (not $ path == "favicon.ico" || Text.isPrefixOf "static" path) $ do
        logRoute site path

-------------------------------------------------------------------------------------
-- server handlers

initRoutes :: Site -> Routes
initRoutes site =
  Routes
    { greeting = handleGreeting site
    , blogPost = handleBlogPost site
    , quote = handleQuote site
    , writeForm = handleWriteForm site
    , writeSubmit = handleWriteSubmit site
    , listPosts = handleListPosts site
    }

-- | Greet the user on main page
handleGreeting :: Site -> GreetingRoute
handleGreeting site =
  Send $ toPage . Greeting <$> site.listBlogPosts

-- | Read blog post by id
handleBlogPost :: Site -> BlogPostRoute
handleBlogPost site (Optional mBlogId) = Send $
  case mBlogId of
    Nothing -> toPage . ViewBlogPost <$> randomBlogPost site
    Just blogId ->
      maybe
        (toErrorPage notFound404 $ PostNotFound blogId)
        (toPage . ViewBlogPost)
        <$> site.readBlogPost blogId

-- | Read random quote
handleQuote :: Site -> QuoteRoute
handleQuote site = Send $ toPage <$> site.readQuote

-- | Show form to the user to fill new post data
handleWriteForm :: Site -> WriteFormRoute
handleWriteForm _site =
  pure $ toPage WritePost

-- | Submit form with data provided by the user
handleWriteSubmit :: Site -> WriteSubmitRoute
handleWriteSubmit site (Body submitData) = Send $ do
  pid <- site.writeBlogPost submitData
  maybe
    (toErrorPage notFound404 $ PostNotFound pid)
    (toPage . ViewBlogPost)
    <$> site.readBlogPost pid

-- | List all posts so far
handleListPosts :: Site -> ListPostsRoute
handleListPosts site = Send $ do
  toPage . ListPosts <$> site.listBlogPosts

-- | Logs the route info
logRoute :: Site -> Text -> IO ()
logRoute site route = do
  site.logInfo $ route <> " page visited"

-- | Get random blog post
randomBlogPost :: Site -> IO BlogPost
randomBlogPost site =
  oneOf =<< site.listBlogPosts

toPage :: (ToMarkup a) => a -> Resp Html
toPage = ok . toMarkup . Page

toErrorPage :: (ToMarkup a) => Status -> a -> Resp Html
toErrorPage status = bad status . toMarkup . Page

-------------------------------------------------------------------------------------
-- utils

-- pick random element from a list
oneOf :: [a] -> IO a
oneOf as = (as !!) . (`mod` len) <$> randomIO
  where
    len = length as
